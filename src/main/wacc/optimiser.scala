package src.main.wacc

import src.main.wacc.builtInFunctions.lb

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

object optimiser {

  def optimise(prog: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    val program =
      AsmProgram(prog) |>
        removePushPop |>
        pushPopToMov
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
      case Push(op) if prog(1) == Pop(op.asInstanceOf[Dest]) =>
        lb(Mov(op, prog(1).asInstanceOf[Pop].dest)) -> 2
      case _ => lb(prog.head) -> 1
    }
  }
}

private case class AsmProgram(instrs: ListBuffer[Instruction]) {

  def apply(i: Int): Instruction = instrs(i)
  def length: Int = instrs.length

  def |>(f: ListBuffer[Instruction] => (ListBuffer[Instruction], Int)): AsmProgram = {
    peepN(instrs, 2, f)
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