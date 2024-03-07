package src.main.wacc

import builtInFunctions.lb
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import peephole._

object optimiser {

  def optimise(
    prog: ListBuffer[Instruction],
    funcs: Map[Ident, ListBuffer[Instruction]]
  ): ListBuffer[Instruction] = {
    val toInline = funcs.filter(inliner.isInlineable)
    val inlined = inliner.nestedInline(prog, toInline)
    val program = inlined
    val optimised = AsmProgram(program) |>
      (removeDeadCode, 1) |>
      (removeZeroAddSub, 1) |>
      (removePushPop, 2) |>
      (pushPopToMov, 2) |>
      (removeJumpToNext, 2) |>
      (removeMovRaxMov, 2) |>
      (removeMovBack, 2)
    optimised.instrs
  }
}

private object inliner {
  // checks if a function should be inlined
  def isInlineable(func: (Ident, ListBuffer[Instruction])): Boolean = {
    func._2.length < 100
  }

  // converts a function body for inlining
  private def convertToInline(
    body: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    val label = Allocator.allocateLabel
    val labels = body.collect { case l: Label => l -> Allocator.allocateLabel }.toMap
    lb(
      Push(Rbp),
      body.tail.map { // remove the label
        case Ret => Jmp(label) // replace returns with jumps
        case Jmp(l) => Jmp(labels.getOrElse(l, l)) // replace jumps with new labels
        case Jo(l) => Jo(labels.getOrElse(l, l))
        case JmpComparison(l, op) => JmpComparison(labels.getOrElse(l, l), op)
        case l: Label => labels(l) // replace labels with new labels
        case inst => inst
      },
      label, // this should be to before the pops, not the the end
      Pop(Rbp)
    )
  }

  private val INLINE_DEPTH = 3

  def nestedInline(
    prog: ListBuffer[Instruction],
    toInline: Map[Ident, ListBuffer[Instruction]]
  ): ListBuffer[Instruction] = {
    var i = 0
    var inlined = prog
    while (i < INLINE_DEPTH) {
      i += 1
      inlined = inline(inlined, toInline)
    }
    inlined
  }

  // inlines a map of functions
  private def inline(
    prog: ListBuffer[Instruction],
    toInline: Map[Ident, ListBuffer[Instruction]]
  ): ListBuffer[Instruction] = {
    prog.flatMap(inst => inst match {
      case CallAsm(label) if toInline.contains(Ident(label.name.stripPrefix("wacc_"))) =>
        convertToInline(toInline(Ident(label.name.stripPrefix("wacc_"))))
      case _ => lb(inst)
    })
  }
}

private object peephole {
  // push x, pop x
  def removePushPop(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    prog.head match {
      case Push(op) if prog(1) == Pop(op.asInstanceOf[Dest]) => lb() -> 2
      case _ => lb(prog.head) -> 1
    }
  }

  // push x, pop y -> mov x, y
  def pushPopToMov(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    prog.head match {
      case Push(op) if prog(1).isInstanceOf[Pop] =>
        lb(Mov(op, prog(1).asInstanceOf[Pop].dest)) -> 2
      case _ => lb(prog.head) -> 1
    }
  }

  // add 0, sub 0
  def removeZeroAddSub(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    prog.head match {
      case AddAsm(Imm(0), _) => lb() -> 1
      case SubAsm(Imm(0), _) => lb() -> 1
      case _ => lb(prog.head) -> 1
    }
  }

  // mov x, rax, mov rax, y -> mov x, y
  def removeMovRaxMov(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    if (prog.length < 2) return lb(prog.head) -> 1
    (prog.head, prog(1)) match {
      case (Mov(op1, op2, _), Mov(op3, op4, _))
        if op2 == Eax(Size64) && op3 == Eax(Size64) &&
          !(op1.isInstanceOf[Address] && op4.isInstanceOf[Address]) =>
        lb(Mov(op1, op4)) -> 2
      case _ => lb(prog.head) -> 1
    }
  }

  private var removing = false
  def removeDeadCode(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    prog.head match {
      case Jmp(_) | Ret if !removing => removing = true; lb(prog.head) -> 1
      case Label(_) => removing = false; lb(prog.head) -> 1
      case _ => (if (removing) lb() else lb(prog.head)) -> 1
    }
  }

  // jmp label, label -> label
  def removeJumpToNext(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    prog.head match {
      case Jmp(label) if prog(1) == label => lb(label) -> 2
      case _ => lb(prog.head) -> 1
    }
  }

  // mov x, y, mov y, x -> mov x, y
  // mov x, y, mov x, y -> mov x, y
  def removeMovBack(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    if (prog.length < 2) return lb(prog.head) -> 1
    lb(prog.head) -> ((prog.head, prog(1)) match {
      case (Mov(op1, op2, _), Mov(op3, op4, _))
        if op1 == op4 && op2 == op3 || op1 == op3 && op2 == op4 => 2
      case (Movs(op1, op2, _, _), Movs(op3, op4, _, _))
        if op1 == op4 && op2 == op3 || op1 == op3 && op2 == op4 => 2
      case _ => 1
    })
  }
}

private case class AsmProgram(instrs: ListBuffer[Instruction]) {

  def apply(i: Int): Instruction = instrs(i)

  // pipe operator for applying peephole optimisations
  def |>(f: ListBuffer[Instruction] => (ListBuffer[Instruction], Int), n: Int): AsmProgram = {
    peepN(instrs, n, f)
  }

  // peephole framework
  private def peepN(
    prog: ListBuffer[Instruction],
    n: Int,
    f: ListBuffer[Instruction] => (ListBuffer[Instruction], Int)
  ): AsmProgram = {
    val newProg = ListBuffer[Instruction]()
    var i = 0
    while (i < prog.length) {
      val (inst, step) = f(prog.slice(i, i+n))
      newProg ++= inst
      i += step
    }
    AsmProgram(newProg)
  }
}