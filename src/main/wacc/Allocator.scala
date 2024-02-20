package src.main.wacc

import scala.collection.mutable.ListBuffer

class Allocator(reservedSpace: Int) {

  private var relativeToBasePointer: Int = -reservedSpace
  private val freeRegs: ListBuffer[Reg] = ListBuffer.from(Allocator.NON_PARAM_REGS)

  def allocateSpace(size: Size): Dest = {
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
  def allocateSpace(t: Type): Dest = t match {
    case CharType | BoolType => allocateSpace(Size8)
    case IntType => allocateSpace(Size32)
    case StringType | ArrayType(_) | PairType(_,_) => allocateSpace(Size64)
  }
}

object Allocator {
  private val PARAM_REGS: List[Reg] =
    List(Edi(Size64), Esi(Size64), Edx(Size64), Ecx(Size64), R8(Size64), R9(Size64))
  val NON_PARAM_REGS: List[Reg] =
    List(R10(Size64), R11(Size64), R12(Size64), R13(Size64), R14(Size64), R15(Size64))

  def apply(vars: List[SymbolTableObj]): Allocator = {
    val reservedSpace = vars.map(x => x.typ.get match {
      case CharType | BoolType => 1
      case IntType => 4
      case StringType | ArrayType(_) | PairType(_,_) => 8
    }).sum
    new Allocator(reservedSpace)
  }
}
