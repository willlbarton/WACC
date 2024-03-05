package src.main.wacc

import builtInFunctions.lb
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

object optimiser {

  def optimise(prog: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    val program =
      AsmProgram(prog) |>
        (removePushPop, 2) |>
        (pushPopToMov, 2) |>
        (removeZeroAddSub, 1)
    program.instrs
  }

  private def removePushPop(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    prog.head match {
      case Push(op) if prog(1) == Pop(op.asInstanceOf[Dest]) => lb() -> 2
      case _ => lb(prog.head) -> 1
    }
  }

  private def pushPopToMov(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
    prog.head match {
      case Push(op) if prog(1).isInstanceOf[Pop] =>
        lb(Mov(op, prog(1).asInstanceOf[Pop].dest)) -> 2
      case _ => lb(prog.head) -> 1
    }
  }

  private def removeZeroAddSub(
    prog: ListBuffer[Instruction]
  ): (ListBuffer[Instruction], Int) = {
    prog.head match {
      case AddAsm(Imm(0), _) => lb() -> 1
      case SubAsm(Imm(0), _) => lb() -> 1
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