package src.main.wacc

import src.main.wacc.generator.lb

import scala.collection.mutable.ListBuffer

object optimiser {

  def optimise(prog: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    peepN(prog, 2, removePushPop)
  }

  private def peepN(prog: ListBuffer[Instruction], n: Int, f: ListBuffer[Instruction] => (ListBuffer[Instruction], Int)): ListBuffer[Instruction] = {
    val newProg = ListBuffer[Instruction]()
    var i = 0
    while (i < prog.length) {
      val (inst, step) = f(prog.slice(i, i+n))
      newProg ++= inst
      i += step
    }
    newProg
  }

  private def removePushPop(prog: ListBuffer[Instruction]): (ListBuffer[Instruction], Int) = {
      prog.head match {
        case Push(op) if prog.last == Pop(op.asInstanceOf[Dest]) => lb() -> 2
        case _ => lb(prog.head) -> 1
      }
  }
}