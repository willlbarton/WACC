package src.main.wacc

import parsley.internal.machine.instructions.Instr
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

<<<<<<< src/main/wacc/Instructions.scala
// Bitwise operators
final case class BitNotAsm(dest: Dest) extends Instruction
final case class BitAndAsm(op: Operand, dest: Dest) extends Instruction
final case class BitOrAsm(op: Operand, dest: Dest) extends Instruction
final case class BitXorAsm(op: Operand, dest: Dest) extends Instruction
final case class BitLeftShiftAsm(op: Operand, dest: Dest) extends Instruction
final case class BitRightShiftAsm(op: Operand, dest: Dest) extends Instruction

trait Formatter {
  def apply(instruction: Instruction): String
  def apply(location: Location): String
}

// x86-64 AT&T instructions
object x86Formatter extends Formatter {

  private lazy val indent = "        "
  override def apply(instruction: Instruction): String = {
    instruction match {
      case Directive(name) =>
        val start = name match {
          case _ if name.startsWith("int") || name.startsWith("asciz") => indent
          case _                                                       => ""
        }
        start ++ s".$name"
      case Label(name) => s"$name:"
      case Ret         => indent ++ "ret\n"
      case Cltd        => indent ++ "cltd"
      case Mov(op1, dest, useOpSize) =>
        indent ++ s"mov${instructionPostfix(if (useOpSize) op1 else dest)}  ${this(op1)}, ${this(dest)}"
      case Movs(op, dest, srcSize, destSize) =>
        indent ++ s"movs${instructionPostfix(srcSize, destSize)} ${this(op)}, ${this(dest)}"
      case Pop(dest)      => indent ++ s"pop${instructionPostfix(dest)}  ${this(dest)}"
      case Push(op1)      => indent ++ s"push${instructionPostfix(op1)} ${this(op1)}"
      case CallAsm(label) => indent ++ s"call  ${label.name}"
      case AndAsm(op1, dest) =>
        indent ++ s"and${instructionPostfix(dest)}   ${this(op1)}, ${this(dest)}"
      case SetAsm(dest, comparison) =>
        indent ++ s"set${instructionPostfix(comparison)} ${this(dest)}"
      case Lea(op1, dest) =>
        indent ++ s"lea${instructionPostfix(dest)}  ${this(op1)}, ${this(dest)}"
      case AddAsm(op1, dest) =>
        indent ++ s"add${instructionPostfix(dest)}  ${this(op1)}, ${this(dest)}"
      case SubAsm(op1, dest) =>
        indent ++ s"sub${instructionPostfix(dest)}  ${this(op1)}, ${this(dest)}"
      case Cmp(src, dest) =>
        indent ++ s"cmp${instructionPostfix(dest)}  ${this(src)}, ${this(dest)}"
      case Jmp(label) => indent ++ s"jmp   ${label.name}"
      case Jo(label)  => indent ++ s"jo    ${label.name}"
      case JmpComparison(label, comparison) =>
        indent ++ s"j${instructionPostfix(comparison)} ${label.name}"
      case Idiv(op1) => indent ++ s"idiv${instructionPostfix(op1)} ${this(op1)}"
      case Imul(op1, dest) =>
        indent ++ s"imul${instructionPostfix(dest)} ${this(op1)}, ${this(dest)}"
      case CMovl(src, dest) =>
        indent ++ s"cmovl ${this(src)}, ${this(dest)}"
      case CMovge(src, dest) =>
        indent ++ s"cmovge ${this(src)}, ${this(dest)}"
      case CMovne(src, dest) =>
        indent ++ s"cmovne ${this(src)}, ${this(dest)}"
      case Testq(op1, op2) => indent ++ s"test  ${this(op1)}, ${this(op2)}"
      // Bitwise ops
      case BitNotAsm(dest) => indent ++ s"not${instructionPostfix(dest)}  ${this(dest)}"
      case BitAndAsm(op, dest) =>
        indent ++ s"and${instructionPostfix(dest)}  ${this(op)}, ${this(dest)}"
      case BitOrAsm(op, dest) =>
        indent ++ s"or${instructionPostfix(dest)}   ${this(op)}, ${this(dest)}"
      case BitXorAsm(op, dest) =>
        indent ++ s"xor${instructionPostfix(dest)}  ${this(op)}, ${this(dest)}"
      case BitLeftShiftAsm(op, dest) =>
        indent ++ s"sal${instructionPostfix(dest)}  ${this(op)}, ${this(dest)}"
      case BitRightShiftAsm(op, dest) =>
        indent ++ s"shr${instructionPostfix(dest)}  ${this(op)}, ${this(dest)}"
    }
  }

  override def apply(location: Location): String = location match {
    case Address(base, offset, index, scale) =>
      val scl = if (scale == Immediate(1)) "" else s", ${memFormat(scale)}"
      val optional = if (index == Immediate(0)) "" else s", ${memFormat(index)}$scl"
      (if (offset == Immediate(0)) "" else s"${memFormat(offset)}") ++
        s"(${memFormat(base)}$optional)"
    case Immediate(value) => s"$$$value"
    case Label(name)      => name
    case reg: Reg         => formatReg(reg)
  }

  private def formatReg(reg: Reg): String = reg match {
    case _: Eax =>
      reg.size match {
        case Size8  => "%al"
        case Size32 => "%eax"
        case Size64 => "%rax"
      }
    case _: Ebx =>
      reg.size match {
        case Size8  => "%bl"
        case Size32 => "%ebx"
        case Size64 => "%rbx"
      }
    case _: Ecx =>
      reg.size match {
        case Size8  => "%cl"
        case Size32 => "%ecx"
        case Size64 => "%rcx"
      }
    case _: Edx =>
      reg.size match {
        case Size8  => "%dl"
        case Size32 => "%edx"
        case Size64 => "%rdx"
      }
    case _: Esi =>
      reg.size match {
        case Size8  => "%sil"
        case Size32 => "%esi"
        case Size64 => "%rsi"
      }
    case _: Edi =>
      reg.size match {
        case Size8  => "%dil"
        case Size32 => "%edi"
        case Size64 => "%rdi"
      }
    case _: R8 =>
      reg.size match {
        case Size8  => "%r8b"
        case Size32 => "%r8d"
        case Size64 => "%r8"
      }
    case _: R9 =>
      reg.size match {
        case Size8  => "%r9b"
        case Size32 => "%r9d"
        case Size64 => "%r9"
      }
    case _: R10 =>
      reg.size match {
        case Size8  => "%r10b"
        case Size32 => "%r10d"
        case Size64 => "%r10"
      }
    case _: R11 =>
      reg.size match {
        case Size8  => "%r11b"
        case Size32 => "%r11d"
        case Size64 => "%r11"
      }
    case _: R12 =>
      reg.size match {
        case Size8  => "%r12b"
        case Size32 => "%r12d"
        case Size64 => "%r12"
      }
    case _: R13 =>
      reg.size match {
        case Size8  => "%r13b"
        case Size32 => "%r13d"
        case Size64 => "%r13"
      }
    case _: R14 =>
      reg.size match {
        case Size8  => "%r14b"
        case Size32 => "%r14d"
        case Size64 => "%r14"
      }
    case _: R15 =>
      reg.size match {
        case Size8  => "%r15b"
        case Size32 => "%r15d"
        case Size64 => "%r15"
      }
    case Rbp => "%rbp"
    case Rsp => "%rsp"
    case Rip => "%rip"
  }

  private def memFormat(mem: MemOp): String = mem match {
    case reg: Reg         => this(reg)
    case Immediate(value) => s"$value"
    case Label(name)      => name
  }

  private def instructionPostfix(location: Location): String = location match {
    case r: Reg =>
      r.size match {
        case Size8  => "b"
        case Size32 => "l"
        case Size64 => "q"
      }
    case _: Address => "q"
    case _          => ???
  }

  private def instructionPostfix(op: Operand, dest: Dest): String =
    instructionPostfix(op) + instructionPostfix(dest)
  private def instructionPostfix(srcSize: Size, destSize: Size): String =
    instructionPostfix(srcSize) + instructionPostfix(destSize)

  private def instructionPostfix(comparison: Comparison) = comparison match {
    case Eq    => "e"
    case Lt    => "l"
    case Gt    => "g"
    case LtEq  => "le"
    case GtEq  => "ge"
    case NotEq => "ne"
  }

  private def instructionPostfix(size: Size): String = size match {
    case Size8  => "b"
    case Size32 => "l"
    case Size64 => "q"
  }

}

=======
>>>>>>> src/main/wacc/Instructions.scala
object constants {
  val byteSize: Int = 1
  val intSize: Int = 4
  val ptrSize: Int = 8
  val boolTrue: Imm = 1
  val badChar: Imm = -128
  val exitSuccess: Imm = 0
  val exitError: Imm = -1
  val nullPtr: Imm = 0
}
