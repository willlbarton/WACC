package src.main.wacc

import builtInFunctions.lb

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import peephole._

object codeOptimiser {

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
      (movPushToPush, 2) |>
      (removeMovsMovAddr, 2) |>
      (simplifyBinApp, 5) |>
      (simplifyUpdate, 5) |>
      (removePushPop, 2) |>
      (removeMovMov, 2)
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

  // don't extend values when moving to addresses
  def removeMovsMovAddr(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    if (prog.length < 2) return lb(prog.head) -> 1
    (prog.head, prog(1)) match {
      case (Movs(op1, Eax(Size64), _, _), m@Mov(op3, _: Address, _)) if op1 == op3 => lb(m) -> 2
      case _ => lb(prog.head) -> 1
    }
  }

  // Remove unnecessary operations when doing binary operations with atomic values
  // We don't need to save the value of the operand in a register
  def simplifyBinApp(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    def getBinApp(
      op1_ : Operand, op2_ : Operand, f: (Operand, Dest) => Instruction, swap: Boolean
    ): (ListBuffer[Instruction], Int) = {
      val op1 = Reg.resize(op1_, Size32)
      val op2 = Reg.resize(op2_, Size32)
      (if (!swap) lb(
        Mov(op1, Eax(Size32), Size32),
        f(op2, Eax(Size32))
      )
      else lb(
        Mov(op1, Ebx(Size32), Size32),
        Mov(op2, Eax(Size32), Size32),
        f(Ebx(Size32), Eax(Size32))
      )) -> 5
    }

    def simplifyApp(v: Operand, op: Operand, i: Instruction, swap: Boolean) = i match {
      case AddAsm(Ebx(Size32), Eax(Size32)) => getBinApp(v, op, AddAsm, swap = false)
      case SubAsm(Ebx(Size32), Eax(Size32)) => getBinApp(v, op, SubAsm, swap = swap)
      case Imul(Ebx(Size32), Eax(Size32))   => getBinApp(v, op, Imul, swap = false)
      case BitAndAsm(Ebx(Size32), Eax(Size32)) => getBinApp(v, op, BitAndAsm, swap = false)
      case BitOrAsm(Ebx(Size32), Eax(Size32)) => getBinApp(v, op, BitOrAsm, swap = false)
      case BitXorAsm(Ebx(Size32), Eax(Size32)) => getBinApp(v, op, BitXorAsm, swap = false)
      case BitLeftShiftAsm(Ebx(Size64), Eax(Size64)) => lb(
        Mov(v, Ecx(Size64), Size64),
        Mov(op, Eax(Size64), Size64),
        BitLeftShiftAsm(Ecx(Size64), Eax(Size64))
      ) -> 5
      case BitRightShiftAsm(Ebx(Size64), Eax(Size64)) => lb(
        Mov(v, Ebx(Size64), Size64),
        Mov(op, Eax(Size64), Size64),
        BitRightShiftAsm(Ebx(Size64), Eax(Size64))
      ) -> 5
      case _ => lb(prog.head) -> 1
    }

    if (prog.length < 5) return lb(prog.head) -> 1
    (prog.head, prog(1), prog(2), prog(3), prog(4)) match {
      case(
        Mov(v: Imm, Eax(Size32), Size32),
        Push(Eax(Size64)),
        Mov(op2, Eax(Size64), _),
        Pop(Ebx(Size64)),
        i) => simplifyApp(v, op2, i, swap = true)
      case (
        Mov(op2, Eax(Size64), _),
        Push(Eax(Size64)),
        Mov(v: Imm, Eax(Size32), Size32),
        Pop(Ebx(Size64)),
        i) => simplifyApp(v, op2, i, swap = false)
      case (
        Push(op1),
        Mov(op2, Eax(Size64), _),
        Pop(Ebx(Size64)),
        i, _) if !op1.isInstanceOf[Eax] =>
          val (instrs, step) = simplifyApp(op2, op1, i, swap = false)
          instrs -> (if (step == 5) 4 else 1)
      case _ => lb(prog.head) -> 1
    }
  }

  // +=, -=, *= Don't use rax, just apply directly
  def simplifyUpdate(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    if (prog.length < 5) return lb(prog.head) -> 1
    (prog.head, prog(1), prog(2), prog(3), prog(4)) match {
      case (
        Mov(op0, Eax(Size32), Size32),
        AddAsm(op1, Eax(Size32)),
        j: Jo,
        Movs(Eax(Size32), op2, Size32, Size64), _
      ) if op1.getClass == op2.getClass =>
        lb(AddAsm(op0, Reg.resize(op2, Size32).asInstanceOf[Dest]), j) -> 4
      case (
        Mov(op0, Ebx(Size32), Size32),
        Mov(op1, Eax(Size32), Size32),
        SubAsm(Ebx(Size32), Eax(Size32)),
        j: Jo,
        Movs(Eax(Size32), op2, Size32, Size64)
        ) if op1.getClass == op2.getClass =>
        lb(SubAsm(op0, Reg.resize(op2, Size32).asInstanceOf[Dest]), j) -> 5
      case (
        Mov(op0, Eax(Size32), Size32),
        Imul(op1, Eax(Size32)),
        j: Jo,
        Movs(Eax(Size32), op2, Size32, Size64), _
        ) if op1.getClass == op2.getClass =>
        lb(Imul(op0, Reg.resize(op2, Size32).asInstanceOf[Dest]), j) -> 4
      case (
        Mov(op1, Eax(Size64), Size64),
        Pop(Ebx(Size64)),
        Mov(Ebx(Size64), Ecx(Size64), Size64),
        BitLeftShiftAsm(Ecx(Size64), Eax(Size64)),
        Mov(Eax(Size64), op2, Size64)
        ) if op1 == op2 => lb(
          Pop(Ecx(Size64)),
          BitLeftShiftAsm(Ecx(Size64), op2),
        ) -> 5
      case _ => lb(prog.head) -> 1
    }
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