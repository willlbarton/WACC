package src.main.wacc

import scala.collection.mutable.ListBuffer
import src.main.wacc.constants._
import src.main.wacc.Imm.intToImmediate

sealed trait Mode
case object ParamMode extends Mode
case object NonParamMode extends Mode

case class Allocator(reservedSpace: Int, mode: Mode) {

  // The current offset from the base pointer
  // This is the next location to allocate space for a variable
  private var relativeToBasePointer: Int = -reservedSpace
  // Available registers to use
  private val freeRegs: ListBuffer[Reg] =
    ListBuffer.from(if (mode == NonParamMode) Allocator.NON_PARAM_REGS else Allocator.PARAM_REGS)

  // Allocate space for a variable of a given size
  def allocateSpace(size: Size): Dest = {
    // If there are available registers, use them
    if (freeRegs.nonEmpty) {
      freeRegs.remove(0)
    } else {
      val currentRelativeBP = relativeToBasePointer
      relativeToBasePointer += (size match {
        case Size8  => byteSize
        case Size32 => intSize
        case Size64 => ptrSize
      })
      Address(Rbp, currentRelativeBP)
    }
  }
  // Allocate space for a variable of a given type
  def allocateSpace(t: Type): Dest = t match {
    case CharType | BoolType                               => allocateSpace(Size8)
    case IntType                                           => allocateSpace(Size32)
    case StringType | ArrayType(_) | PairType(_, _) | Pair => allocateSpace(Size64)
    case NullType => throw new IllegalArgumentException("NullType should not be allocated")
  }

  // Registers that have been allocated
  def usedRegs: List[Reg] =
    if (mode == NonParamMode)
      Allocator.NON_PARAM_REGS.take(Allocator.NON_PARAM_REGS.length - freeRegs.length)
    else Allocator.PARAM_REGS.take(Allocator.PARAM_REGS.length - freeRegs.length)
}

object Allocator {

  // Registers that are used for parameters
  val PARAM_REGS: List[Reg] =
    List(Edi(Size64), Esi(Size64), Edx(Size64), Ecx(Size64), R8(Size64), R9(Size64))
  // Registers that are used for non-parameters
  val NON_PARAM_REGS: List[Reg] =
    List(R12(Size64), R13(Size64), R14(Size64), R15(Size64))

  // Current next label number
  var label = 0

  // Create an allocator for a list of variables
  def apply(vars: List[SymbolTableObj], mode: Mode): Allocator = {
    val stackVars =
      vars.drop(if (mode == NonParamMode) NON_PARAM_REGS.length else PARAM_REGS.length)
    val reservedSpace = stackVars.map(x => getTypeWidth(x.typ.get)).sum

    new Allocator(reservedSpace, mode)
  }
  def apply(vars: List[SymbolTableObj]): Allocator = {
    apply(vars, NonParamMode)
  }

  // Get the width of a type
  def getTypeWidth(t: Type): Int = t match {
    case CharType | BoolType                               => byteSize
    case IntType                                           => intSize
    case StringType | ArrayType(_) | PairType(_, _) | Pair => ptrSize
    case NullType => throw new IllegalArgumentException("NullType should not be allocated")
  }
  // Get the size of a type
  def getTypeSize(t: Type): Size = t match {
    case CharType | BoolType                               => Size8
    case IntType                                           => Size32
    case StringType | ArrayType(_) | PairType(_, _) | Pair => Size64
    case NullType => throw new IllegalArgumentException("NullType should not be allocated")
  }
  // Get the next label
  def allocateLabel: Label = {
    val oldLabel = label
    label += 1
    Label(s".L$oldLabel")
  }
}
