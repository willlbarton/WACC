import scala.collection.mutable.ListBuffer
import src.main.wacc._

object Allocater {
  private val PARAM_REGS: List[Reg] = List(Edi(), Esi(), Edx(), Ecx(), R8(), R9())
  private val NON_PARAM_REGS: List[Reg] = List(Ebx(), R10(), R11(), R12(), R13(), R14(), R15())

  var relativeToBasePointer: Int = 0;
  var freeRegs: ListBuffer[Reg] = ListBuffer.empty

  def allocateSpace(size: Size): Location = {
    if (freeRegs.nonEmpty) {
      val reg = freeRegs.remove(0)
      reg
    }

    val currentRelaltiveBP = relativeToBasePointer
    relativeToBasePointer.+(size match {
      case Size8  => 1
      case Size16 => 2
      case Size32 => 4
      case Size64 => 8
    })
    Address(Immediate(currentRelaltiveBP.toLong), Rbp)
  }

  def resetAllocater(reservedSpace: Int) = {
    relativeToBasePointer = -reservedSpace;
    freeRegs = ListBuffer.from(NON_PARAM_REGS)
  }
}
