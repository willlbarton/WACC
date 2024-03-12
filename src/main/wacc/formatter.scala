package src.main.wacc

import scala.collection.mutable.ListBuffer

object formatter {
  def format(prog: ListBuffer[Instruction], formatter: AsmFormatter): String =
    prog.map(formatter(_))
      .mkString("\n") + "\n"
}

trait AsmFormatter {
  // Format an instruction
  def apply(instruction: Instruction): String
  // Format an address, immediate, label or register
  def apply(location: Location): String
}

// x86-64 AT&T instructions
object x86Formatter extends AsmFormatter {

  private lazy val intDirStart = "int"
  private lazy val stringDirStart = "asciz"
  private lazy val indent = "        "
  override def apply(instruction: Instruction): String = {
    instruction match {
      case Directive(name) =>
        val start = name match {
          case _ if name.startsWith(intDirStart) || name.startsWith(stringDirStart) => indent
          case _ => ""
        }
        s"$start.$name"
      case Label(name) => s"$name:"
      case Ret         => indent ++ "ret\n"
      case Cltd        => indent ++ "cltd"
      case Mov(op1, dest, size) =>
        indent ++ s"mov${instructionPostfix(size)}  ${this(op1)}, ${this(dest)}"
      case Movs(op, dest, srcSize, destSize) =>
        indent ++ s"movs${instructionPostfix(srcSize, destSize)} ${this(op)}, ${this(dest)}"
      case Pop(dest)      => indent ++ s"pop${instructionPostfix(dest)}  ${this(dest)}"
      case Push(op1)      => indent ++ s"push${instructionPostfix(op1)} ${this(op1)}"
      case CallAsm(label, _) => indent ++ s"call  ${label.name}"
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
      val scl = if (scale == Imm(1)) "" else s", ${memFormat(scale)}"
      val optional = if (index == Imm(0)) "" else s", ${memFormat(index)}$scl"
      (if (offset == Imm(0)) "" else s"${memFormat(offset)}") ++
        s"(${memFormat(base)}$optional)"
    case Imm(value)  => s"$$$value"
    case Label(name) => name
    case reg: Reg    => formatReg(reg)
  }

  private def formatReg(reg: Reg): String = reg match {
    case _: Eax => reg.size match {
      case Size8  => "%al"
      case Size32 => "%eax"
      case Size64 => "%rax"
    }
    case _: Ebx => reg.size match {
      case Size8  => "%bl"
      case Size32 => "%ebx"
      case Size64 => "%rbx"
    }
    case _: Ecx => reg.size match {
      case Size8  => "%cl"
      case Size32 => "%ecx"
      case Size64 => "%rcx"
    }
    case _: Edx => reg.size match {
      case Size8  => "%dl"
      case Size32 => "%edx"
      case Size64 => "%rdx"
    }
    case _: Esi => reg.size match {
      case Size8  => "%sil"
      case Size32 => "%esi"
      case Size64 => "%rsi"
    }
    case _: Edi => reg.size match {
      case Size8  => "%dil"
      case Size32 => "%edi"
      case Size64 => "%rdi"
    }
    case _: R8 => reg.size match {
      case Size8  => "%r8b"
      case Size32 => "%r8d"
      case Size64 => "%r8"
    }
    case _: R9 => reg.size match {
      case Size8  => "%r9b"
      case Size32 => "%r9d"
      case Size64 => "%r9"
    }
    case _: R10 => reg.size match {
      case Size8  => "%r10b"
      case Size32 => "%r10d"
      case Size64 => "%r10"
    }
    case _: R11 => reg.size match {
      case Size8  => "%r11b"
      case Size32 => "%r11d"
      case Size64 => "%r11"
    }
    case _: R12 => reg.size match {
      case Size8  => "%r12b"
      case Size32 => "%r12d"
      case Size64 => "%r12"
    }
    case _: R13 => reg.size match {
      case Size8  => "%r13b"
      case Size32 => "%r13d"
      case Size64 => "%r13"
    }
    case _: R14 => reg.size match {
      case Size8  => "%r14b"
      case Size32 => "%r14d"
      case Size64 => "%r14"
    }
    case _: R15 => reg.size match {
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
    case Imm(value)       => s"$value"
    case Label(name)      => name
  }

  private def instructionPostfix(location: Location): String = location match {
    case r: Reg => r.size match {
      case Size8  => "b"
      case Size32 => "l"
      case Size64 => "q"
    }
    case _: Address => "q" // Default to 64 bit
    case _          => throw new IllegalArgumentException(s"Invalid postfix argument: $location")
  }

  private def instructionPostfix(srcSize: Size, destSize: Size): String =
    instructionPostfix(srcSize) + instructionPostfix(destSize)

  private def instructionPostfix(comparison: Comparison): String = comparison match {
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