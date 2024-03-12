package src.main.wacc

import scala.language.implicitConversions
import Imm.intToImmediate

sealed trait Instruction

sealed trait Location {
  val size: Size
}
sealed trait Operand extends Location
sealed trait Dest extends Location with Operand
sealed trait MemOp extends Location

sealed trait Reg extends Dest with MemOp
object Reg {
  def resize(op: Operand, size: Size): Operand = op match {
    case _: Eax => Eax(size)
    case _: Ebx => Ebx(size)
    case _: Esi => Esi(size)
    case _: Edi => Edi(size)
    case _: Edx => Edx(size)
    case _: Ecx => Ecx(size)
    case _: R8 => R8(size)
    case _: R9 => R9(size)
    case _: R10 => R10(size)
    case _: R11 => R11(size)
    case _: R12 => R12(size)
    case _: R13 => R13(size)
    case _: R14 => R14(size)
    case _: R15 => R15(size)
    case _: Address => op
    case _: Imm => op
    case _ => throw new IllegalArgumentException("Bad operand for resize")
  }
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
    offset: MemOp = 0,
    index: MemOp = 0,
    scale: MemOp = 1
) extends Dest {
  override val size: Size = Size64
}

case class Imm(value: Int) extends Operand with MemOp {
  override val size: Size = Size64
}
case object Imm {
  implicit def intToImmediate(value: Int): Imm = Imm(value)
}

case object Ret extends Instruction
case object Cltd extends Instruction

final case class Directive(name: String) extends Instruction
final case class Label(name: String) extends Instruction with MemOp {
  override val size: Size = Size64
}
final case class Mov(op: Operand, dest: Dest, size: Size) extends Instruction
case object Mov {
  def apply(op: Operand, dest: Dest, useOpSize : Boolean = false): Mov =
    Mov(op, dest, if (useOpSize) op.size else dest.size)
}
final case class Movs(op: Operand, dest: Dest, srcSize: Size, destSize: Size) extends Instruction
final case class Pop(dest: Dest) extends Instruction
final case class Push(op: Operand) extends Instruction
final case class CallAsm(label: Label) extends Instruction
final case class AndAsm(op: Operand, dest: Dest) extends Instruction
final case class Lea(op: Address, dest: Dest) extends Instruction

final case class SetAsm(dest: Dest, comparison: Comparison) extends Instruction

final case class AddAsm(op: Operand, dest: Dest) extends Instruction
final case class SubAsm(op: Operand, dest: Dest) extends Instruction
final case class Cmp(op1: Operand, op2: Operand) extends Instruction

final case class Jmp(label: Label) extends Instruction
final case class Jo(label: Label) extends Instruction

final case class JmpComparison(label: Label, comparison: Comparison) extends Instruction

final case class Idiv(op: Operand) extends Instruction
final case class Imul(op1: Operand, dest: Dest) extends Instruction

final case class Testq(op1: Operand, op2: Operand) extends Instruction
final case class CMovl(op: Operand, dest: Dest) extends Instruction
final case class CMovge(op: Operand, dest: Dest) extends Instruction
final case class CMovne(op: Operand, dest: Dest) extends Instruction

// Bitwise operators
final case class BitNotAsm(dest: Dest) extends Instruction
final case class BitAndAsm(op: Operand, dest: Dest) extends Instruction
final case class BitOrAsm(op: Operand, dest: Dest) extends Instruction
final case class BitXorAsm(op: Operand, dest: Dest) extends Instruction
final case class BitLeftShiftAsm(op: Operand, dest: Dest) extends Instruction
final case class BitRightShiftAsm(op: Operand, dest: Dest) extends Instruction

object constants {
  val byteSize: Int = 1
  val intSize: Int = 4
  val ptrSize: Int = 8
  val boolTrue: Imm = 1
  val exitSuccess: Imm = 0
  val exitError: Imm = -1
  val nullPtr: Imm = 0
}
