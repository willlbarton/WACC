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
        case Size8  => 1
        case Size16 => 2
        case Size32 => 4
        case Size64 => 8
      })
      Address(Rbp, Immediate(currentRelativeBP.toLong))
    }
  }
  def allocateSpace(t: Type): Dest = t match {
    case CharType | BoolType                        => allocateSpace(Size8)
    case IntType                                    => allocateSpace(Size32)
    case StringType | ArrayType(_) | PairType(_, _) => allocateSpace(Size64)
  }
}

/*
In x86-64 assembly, r12, r13, r14, r15, rbx, rsp, and rbp are callee-saved registers.
Rax, rdi, rsi, rdx, rcx, r8, r9, r10, and r11 are caller-saved registers.

Functions like printf@plt overwrite r11 and r12
 */
object Allocator {
  private val PARAM_REGS: List[Reg] =
    List(Edi(Size64), Esi(Size64), Edx(Size64), Ecx(Size64), R8(Size64), R9(Size64))
  val NON_PARAM_REGS: List[Reg] =
    List( /*R10(Size64), R11(Size64),*/ R12(Size64), R13(Size64), R14(Size64), R15(Size64))

  var label = 0

  def apply(vars: List[SymbolTableObj]): Allocator = {
    val stackVars = vars.drop(NON_PARAM_REGS.length)
    val reservedSpace = stackVars
      .map(x =>
        x.typ.get match {
          case CharType | BoolType                        => 1
          case IntType                                    => 4
          case StringType | ArrayType(_) | PairType(_, _) => 8
        }
      )
      .sum
    new Allocator(reservedSpace)
  }

  def allocateLabel: Label = {
    val oldLabel = label;
    label += 1
    Label(s".L$oldLabel")
  }
}
