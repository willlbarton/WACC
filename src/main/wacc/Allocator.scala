package src.main.wacc

import scala.collection.mutable.ListBuffer
import src.main.wacc.constants._

sealed trait Mode
case object ParamMode extends Mode
case object NonParamMode extends Mode

case class Allocator(reservedSpace: Int, mode: Mode) {

  private var relativeToBasePointer: Int = -reservedSpace
  private val freeRegs: ListBuffer[Reg] =
    ListBuffer.from(if (mode == NonParamMode) Allocator.NON_PARAM_REGS else Allocator.PARAM_REGS)

  def allocateSpace(size: Size): Dest = {
    if (freeRegs.nonEmpty) {
      freeRegs.remove(0)
    } else {
      val currentRelativeBP = relativeToBasePointer
      relativeToBasePointer += (size match {
        case Size8  => byteSize
        case Size32 => intSize
        case Size64 => ptrSize
      })
      Address(Rbp, Immediate(currentRelativeBP))
    }
  }
  def allocateSpace(t: Type): Dest = t match {
    case CharType | BoolType                               => allocateSpace(Size8)
    case IntType                                           => allocateSpace(Size32)
    case StringType | ArrayType(_) | PairType(_, _) | Pair => allocateSpace(Size64)
    case NullType => throw new IllegalArgumentException("NullType should not be allocated")
  }

  def usedRegs: List[Reg] =
    if (mode == NonParamMode)
      Allocator.NON_PARAM_REGS.take(Allocator.NON_PARAM_REGS.length - freeRegs.length)
    else Allocator.PARAM_REGS.take(Allocator.PARAM_REGS.length - freeRegs.length)
}

/*
In x86-64 assembly, r12, r13, r14, r15, rbx, rsp, and rbp are callee-saved registers.
Rax, rdi, rsi, rdx, rcx, r8, r9, r10, and r11 are caller-saved registers.

Functions like printf@plt overwrite r10 and r11
 */
object Allocator {

  val PARAM_REGS: List[Reg] =
    List(Edi(Size64), Esi(Size64), Edx(Size64), Ecx(Size64), R8(Size64), R9(Size64))
  val NON_PARAM_REGS: List[Reg] =
    List( /*R10(Size64), R11(Size64),*/ R12(Size64), R13(Size64), R14(Size64), R15(Size64))

  var label = 0

  def apply(vars: List[SymbolTableObj], mode: Mode): Allocator = {
    val stackVars =
      vars.drop(if (mode == NonParamMode) NON_PARAM_REGS.length else PARAM_REGS.length)
    val reservedSpace = stackVars.map(x => getTypeWidth(x.typ.get)).sum

    new Allocator(reservedSpace, mode)
  }

  def apply(vars: List[SymbolTableObj]): Allocator = {
    apply(vars, NonParamMode)
  }

  def getTypeWidth(t: Type): Int = t match {
    case CharType | BoolType                               => byteSize
    case IntType                                           => intSize
    case StringType | ArrayType(_) | PairType(_, _) | Pair => ptrSize
    case NullType => throw new IllegalArgumentException("NullType should not be allocated")
  }

  def getTypeSize(t: Type) = t match {
    case CharType | BoolType                               => Size8
    case IntType                                           => Size32
    case StringType | ArrayType(_) | PairType(_, _) | Pair => Size64
    case NullType => throw new IllegalArgumentException("NullType should not be allocated")
  }

  def allocateLabel: Label = {
    val oldLabel = label
    label += 1
    Label(s".L$oldLabel")
  }
}
