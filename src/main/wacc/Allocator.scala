import scala.collection.mutable.ListBuffer
import src.main.wacc._

class Allocator(reservedSpace: Int) {

  private var relativeToBasePointer: Int = -reservedSpace
  private val freeRegs: ListBuffer[Reg] = ListBuffer.from(Allocator.NON_PARAM_REGS)

  def allocateSpace(size: Size): Location = {
    if (freeRegs.nonEmpty) {
      freeRegs.remove(0)
    } else {
      val currentRelativeBP = relativeToBasePointer
      relativeToBasePointer += (size match {
        case Size8 => 1
        case Size16 => 2
        case Size32 => 4
        case Size64 => 8
      })
      Address(Immediate(currentRelativeBP.toLong), Rbp)
    }
  }
}

object Allocator {
  private val PARAM_REGS: List[Reg] = List(Edi(), Esi(), Edx(), Ecx(), R8(), R9())
  private val NON_PARAM_REGS: List[Reg] = List(R10(), R11(), R12(), R13(), R14(), R15())
}
