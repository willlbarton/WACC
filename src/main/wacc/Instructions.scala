package src.main.wacc

sealed trait Instruction

sealed trait Location
sealed trait Operand extends Location
sealed trait Dest extends Location with Operand
sealed trait MemOp extends Location

sealed trait Reg extends Dest with MemOp {
  val size: Size
}

sealed trait Size
case object Size8 extends Size
case object Size32 extends Size
case object Size64 extends Size

// Registers
case class Eax(override val size: Size = Size32) extends Reg
// Param registers
case class Edi(override val size: Size = Size32) extends Reg
case class Esi(override val size: Size = Size32) extends Reg
case class Edx(override val size: Size = Size32) extends Reg
case class Ecx(override val size: Size = Size32) extends Reg
case class R8(override val size: Size = Size32) extends Reg
case class R9(override val size: Size = Size32) extends Reg
// Non-param registers
case class Ebx(override val size: Size = Size32) extends Reg
case class R10(override val size: Size = Size32) extends Reg
case class R11(override val size: Size = Size32) extends Reg
case class R12(override val size: Size = Size32) extends Reg
case class R13(override val size: Size = Size32) extends Reg
case class R14(override val size: Size = Size32) extends Reg
case class R15(override val size: Size = Size32) extends Reg
// Stack pointer and base pointer
case object Rbp extends Reg { override val size: Size = Size64 }
case object Rsp extends Reg { override val size: Size = Size64 }
case object Rip extends Reg { override val size: Size = Size64 }

final case class Address(
    base: MemOp,
    offset: MemOp = Immediate(0),
    index: MemOp = Immediate(0),
    scale: MemOp = Immediate(1)
) extends Dest
final case class Immediate(value: Int) extends Operand with MemOp

case object Ret extends Instruction
case object Cltd extends Instruction

final case class Directive(name: String) extends Instruction
final case class Label(name: String) extends Instruction with MemOp
final case class Mov(op: Operand, dest: Dest, useOpSize: Boolean = false) extends Instruction
final case class Movs(op: Operand, dest: Dest, srcSize: Size, destSize: Size) extends Instruction
final case class Pop(dest: Dest) extends Instruction
final case class Push(op: Operand) extends Instruction
final case class CallAsm(label: Label) extends Instruction
final case class AndAsm(op: Operand, dest: Dest) extends Instruction
final case class Lea(op: Address, dest: Dest) extends Instruction

final case class SetAsm(dest: Dest, comparison: Comparison) extends Instruction

final case class AddAsm(op: Operand, dest: Dest) extends Instruction
final case class SubAsm(op: Operand, dest: Dest) extends Instruction
final case class Cmp(src: Operand, dest: Dest) extends Instruction

final case class Jmp(label: Label) extends Instruction
final case class Jo(label: Label) extends Instruction

final case class JmpComparison(label: Label, comparison: Comparison) extends Instruction

final case class Idiv(op: Operand) extends Instruction
final case class Imul(op1: Operand, dest: Dest) extends Instruction

final case class Testq(op1: Operand, op2: Operand) extends Instruction
final case class CMovl(op: Operand, dest: Dest) extends Instruction
final case class CMovge(op: Operand, dest: Dest) extends Instruction
final case class CMovne(op: Operand, dest: Dest) extends Instruction

object constants {
  val byteSize: Int = 1
  val intSize: Int = 4
  val ptrSize: Int = 8
  val boolTrue: Immediate = Immediate(1)
  val badChar: Immediate = Immediate(-128)
  val exitSuccess: Immediate = Immediate(0)
  val exitError: Immediate = Immediate(-1)
  val nullPtr: Immediate = Immediate(0)
}
