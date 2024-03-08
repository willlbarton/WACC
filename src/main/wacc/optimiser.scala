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
    val inlined = inliner.inline(prog, toInline)
    val program = inlined
    val optimised = AsmProgram(program) |>
      (removeDeadCode, 1) |>
      (removeZeroAddSub, 1) |>
      (removeJumpToNext, 2) |>
      (removePushPop, 2) |>
      (removeMovMov, 2) |>
      (movPushToPush, 2) |>
      (removeBinaryImmPushPop, 5)
    optimised.instrs
  }
}

private object inliner {
  // checks if a function should be inlined
  def isInlineable(func: (Ident, ListBuffer[Instruction])): Boolean = {
    func._2.length < 25
  }

  // converts a function body for inlining
  private def convertToInline(
    body: ListBuffer[Instruction],
    toInline: Map[Ident, ListBuffer[Instruction]]
  ): ListBuffer[Instruction] = {
    val label = Allocator.allocateLabel
    val labels = body.collect { case l: Label => l -> Allocator.allocateLabel }.toMap
    var instructions = lb(
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
    var i = 0
    while (i < INLINE_DEPTH) {
      i += 1
      instructions = inline(instructions, toInline)
    }
    instructions
  }

  private val INLINE_DEPTH = 2

  // inlines a map of functions
  def inline(
    prog: ListBuffer[Instruction],
    toInline: Map[Ident, ListBuffer[Instruction]]
  ): ListBuffer[Instruction] = {
    prog.flatMap(inst => inst match {
      case CallAsm(label) if toInline.contains(Ident(label.name.stripPrefix("wacc_"))) =>
        convertToInline(toInline(Ident(label.name.stripPrefix("wacc_"))), toInline)
      case _ => lb(inst)
    })
  }
}

private object peephole {
  // push x, pop x
  // push x, pop y -> mov x, y
  def removePushPop(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    if (prog.length < 2) return lb(prog.head) -> 1
    (prog.head, prog(1)) match {
      case (Push(op), Pop(op2)) => (if (op == op2) lb() else lb(Mov(op, op2))) -> 2
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
  // mov x, rax, mov rax, y -> mov x, y
  def removeMovMov(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    if (prog.length < 2) return lb(prog.head) -> 1
    (prog.head, prog(1)) match {
      case (Mov(op1, op2, _), Mov(op3, op4, _))
        if op1 == op4 && op2 == op3 || op1 == op3 && op2 == op4 => lb(prog.head) -> 2
      case (Movs(op1, op2, _, _), Movs(op3, op4, _, _))
        if op1 == op4 && op2 == op3 || op1 == op3 && op2 == op4 => lb(prog.head) -> 2
      case (Mov(op1, Eax(_), _), Mov(Eax(s2), op4, s1))
        if !(op1.isInstanceOf[Address] && op4.isInstanceOf[Address]) =>
        lb(Mov(op1, op4, if (op4.isInstanceOf[Address]) s2 else s1)) -> 2
      case (Movs(op1, Eax(_), s1, _), Movs(Eax(Size64), op4, _, s2))
        if !(op1.isInstanceOf[Address] && op4.isInstanceOf[Address]) =>
        lb(Movs(op1, op4, s1, s2)) -> 2
      case (Mov(op1, Eax(_), s1), Movs(Eax(Size64), op4, _, s2))
        if !(op1.isInstanceOf[Address] && op4.isInstanceOf[Address]) =>
        lb(Movs(op1, op4, s1, s2)) -> 2
      case (Movs(op1, Eax(_), s1, Size64), Mov(Eax(Size64), op4, s2))
        if !(op1.isInstanceOf[Address] && op4.isInstanceOf[Address]) =>
        lb(Movs(op1, op4, s1, s2)) -> 2
      case _ => lb(prog.head) -> 1
    }
  }

  // mov x, rax, push rax -> push x
  def movPushToPush(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    if (prog.length < 2) return lb(prog.head) -> 1
    (prog.head, prog(1)) match {
      case (Mov(op1, Eax(Size64), _), Push(Eax(Size64))) if !op1.isInstanceOf[Imm] =>
        lb(Push(op1)) -> 2
      case _ => lb(prog.head) -> 1
    }
  }

  // Remove unnecessary operations when doing binary operations with immediate values
  def removeBinaryImmPushPop(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    def binImm(v: Int, operand: Operand, f: (Operand, Dest) => Instruction, swap: Boolean) = {
      val op = operand match {
        case _: R12 => R12(Size32)
        case _: R13 => R13(Size32)
        case _: R14 => R14(Size32)
        case _: R15 => R15(Size32)
        case a: Address => a
        case _ => throw new IllegalArgumentException("Bad operand for binary imm optimisation")
      }
      (if (!swap) lb(
          Mov(Imm(v), Eax(Size32), Size32),
          f(op, Eax(Size32))
        )
      else lb(
          Mov(Imm(v), Ebx(Size32), Size32),
          Mov(op, Eax(Size32), Size32),
          f(Ebx(Size32), Eax(Size32))
        )
      )-> 5
    }

    def simplifyApp(v: Int, op: Operand, i: Instruction, swap: Boolean) = i match {
      case AddAsm(Ebx(Size32), Eax(Size32)) => binImm(v, op, AddAsm, swap = false)
      case SubAsm(Ebx(Size32), Eax(Size32)) => binImm(v, op, SubAsm, swap = swap)
      case Imul(Ebx(Size32), Eax(Size32))   => binImm(v, op, Imul, swap = false)
      case _ => lb(prog.head) -> 1
    }

    if (prog.length < 5) return lb(prog.head) -> 1
    (prog.head, prog(1), prog(2), prog(3), prog(4)) match {
      case(
        Mov(Imm(v), Eax(Size32), Size32),
        Push(Eax(Size64)),
        Mov(op, Eax(Size64), _),
        Pop(Ebx(Size64)),
        i) => simplifyApp(v, op, i, swap = true)
      case (
        Mov(op, Eax(Size64), _),
        Push(Eax(Size64)),
        Mov(Imm(v), Eax(Size32), Size32),
        Pop(Ebx(Size64)),
        i) => simplifyApp(v, op, i, swap = false)
      case _ => lb(prog.head) -> 1
    }
  }

  // don't extend values when moving to addresses
  // ...
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