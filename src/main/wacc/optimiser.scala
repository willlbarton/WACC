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
    val inlined = inliner.inline(prog, inliner.convertToInlineMap(funcs))
    val program = AsmProgram(inlined) |>
      (removePushPop, 2) |>
      (pushPopToMov, 2) |>
      (removeZeroAddSub, 1) |>
      (removeDeadCode, 1) |>
      (removeJumpToNext, 2) |>
      (removeMovRaxMov, 2)
    program.instrs
  }
}

private object inliner {
  def convertToInlineMap(
    funcs: Map[Ident, ListBuffer[Instruction]]
  ): Map[Ident, ListBuffer[Instruction]] = {
    funcs.map { case (k,v) =>
      val label = Allocator.allocateLabel
      k -> lb(
        v.tail.map { // remove the label
          case Ret => Jmp(label) // replace returns with jumps
          case x => x
        },
        label // this should be to before the pops, not the the end
      )
    }
  }

  def inline(prog: ListBuffer[Instruction], funcs: Map[Ident, ListBuffer[Instruction]]): ListBuffer[Instruction] = {
    val toInline = funcs.filter { case (_, v) => v.length < 30 } // TODO: find a good heuristic
    prog.flatMap(inst => inst match {
      case CallAsm(label) if toInline.contains(Ident(label.name.stripPrefix("wacc_"))) =>
        toInline(Ident(label.name.stripPrefix("wacc_")))
      case _ => lb(inst)
    })
  }
}

private object peephole {
  def removePushPop(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    prog.head match {
      case Push(op) if prog(1) == Pop(op.asInstanceOf[Dest]) => lb() -> 2
      case _ => lb(prog.head) -> 1
    }
  }

  def pushPopToMov(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    prog.head match {
      case Push(op) if prog(1).isInstanceOf[Pop] =>
        lb(Mov(op, prog(1).asInstanceOf[Pop].dest)) -> 2
      case _ => lb(prog.head) -> 1
    }
  }

  def removeZeroAddSub(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    prog.head match {
      case AddAsm(Imm(0), _) => lb() -> 1
      case SubAsm(Imm(0), _) => lb() -> 1
      case _ => lb(prog.head) -> 1
    }
  }

  def removeMovRaxMov(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    if (prog.length < 2) return lb(prog.head) -> 1
    (prog.head, prog(1)) match {
      case (Mov(op1, op2, _), Mov(op3, op4, _))
        if op2 == Eax(Size64) && op3 == Eax(Size64) =>
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

  def removeJumpToNext(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    prog.head match {
      case Jmp(label) if prog(1) == label => lb(label) -> 2
      case _ => lb(prog.head) -> 1
    }
  }
}

private case class AsmProgram(instrs: ListBuffer[Instruction]) {

  def apply(i: Int): Instruction = instrs(i)

  def |>(f: ListBuffer[Instruction] => (ListBuffer[Instruction], Int), n: Int): AsmProgram = {
    peepN(instrs, n, f)
  }

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