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
    val inlined = inliner.inline(prog, funcs)
    val program =
      AsmProgram(inlined) |>
        (removePushPop, 2) |>
        (pushPopToMov, 2) |>
        (removeZeroAddSub, 1)
    program.instrs
  }
}

private object inliner {
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
      case Push(op) if prog(1) == Pop(op.asInstanceOf[Dest]) =>
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
}

private case class AsmProgram(instrs: ListBuffer[Instruction]) {

  def apply(i: Int): Instruction = instrs(i)
  def length: Int = instrs.length

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