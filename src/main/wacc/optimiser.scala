package src.main.wacc

import scala.collection.mutable.ListBuffer

object optimiser {

  def optimise(prog: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    removePushPop(prog)
  }

  private def removePushPop(prog: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    val newProg = ListBuffer[Instruction]()
    var i = 0
    while (i < prog.length) {
      prog(i) match {
        case Push(op) if prog(i+1) == Pop(op.asInstanceOf[Dest]) => i += 2
        case _ => newProg += prog(i)
          i += 1
      }
    }
    newProg
  }
}