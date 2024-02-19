package src.main.wacc

sealed trait Instruction

sealed trait Location
sealed trait Dest extends Location
sealed trait Operand extends Location
sealed trait MemOp extends Location

sealed trait Reg extends Dest with Operand with MemOp {
  var size: Size = Size32;
}

sealed trait Size
case object Size1 extends Size
case object Size8 extends Size
case object Size16 extends Size
case object Size32 extends Size
case object Size64 extends Size

// Registers
case object Rax extends Reg
// Param registers
case object Rdi extends Reg
case object Rsi extends Reg
case object Rdx extends Reg
case object Rcx extends Reg
case object R8 extends Reg
case object R9 extends Reg
// Non-param registers
case object Rbx extends Reg
case object R10 extends Reg
case object R11 extends Reg
case object R12 extends Reg
case object R13 extends Reg
case object R14 extends Reg
case object R15 extends Reg
// Stack pointer and base pointer
case object Rbp extends Reg
case object Rsp extends Reg

final case class Address(
    offset: MemOp = Immediate(0),
    base: MemOp,
    index: MemOp = Immediate(0),
    scale: MemOp = Immediate(1)
) extends Dest
    with Operand
final case class Immediate(value: Long) extends Operand with MemOp

case object Ret extends Instruction
case object Cltd extends Instruction

final case class Directive(name: String) extends Instruction
final case class Label(name: String) extends Instruction with MemOp
final case class Mov(dest: Dest, op: Operand) extends Instruction
final case class Pop(dest: Dest) extends Instruction
final case class Push(op: Operand) extends Instruction
final case class CallAsm(label: Label) extends Instruction
final case class AndAsm(dest: Dest, op: Operand) extends Instruction
final case class Setne(dest: Dest) extends Instruction
final case class Lea(dest: Dest, op: Address) extends Instruction

final case class AddAsm(dest: Reg, op: Operand) extends Instruction
final case class SubAsm(dest: Reg, op: Operand) extends Instruction
final case class Cmp(op1: Operand, op2: Operand) extends Instruction

final case class Jmp(label: Label) extends Instruction
final case class Je(label: Label) extends Instruction
final case class Jl(label: Label) extends Instruction
final case class Jo(label: Label) extends Instruction
final case class Jne(label: Label) extends Instruction
final case class Idiv(op: Operand) extends Instruction

trait Formatter {
  def apply(instruction: Instruction): String
  def apply(reg: Reg): String
  def apply(location: Location): String
}

// x86-64 AT&T instructions
object x86Formatter extends Formatter {
  override def apply(instruction: Instruction): String = {
    instruction match {
      case Directive(name)   => s".$name"
      case Label(name)       => s"$name:"
      case Ret               => "  ret\n"
      case Cltd              => "  cltd"
      case Mov(dest, op1)    => s"  mov${instructionPostfix(dest)}  ${this(op1)}, ${this(dest)}"
      case Pop(dest)         => s"  pop${instructionPostfix(dest)}  ${this(dest)}"
      case Push(op1)         => s"  push${instructionPostfix(op1)} ${this(op1)}"
      case CallAsm(label)    => s"  call  ${label.name}"
      case AndAsm(dest, op1) => s"  and   ${this(op1)}, ${this(dest)}"
      case Setne(dest)       => s"  setne ${this(dest)}"
      case Lea(dest, op1)    => s"  lea${instructionPostfix(dest)}  ${this(op1)}, ${this(dest)}"
      case AddAsm(dest, op1) => s"  add${instructionPostfix(dest)}  ${this(op1)}, ${this(dest)}"
      case SubAsm(dest, op1) => s"  sub${instructionPostfix(dest)}  ${this(op1)}, ${this(dest)}"
      case Cmp(op1, op2)     => s"  cmp${instructionPostfix(op1)}  ${this(op1)}, ${this(op2)}"
      case Jmp(label)        => s"  jmp   ${label.name}"
      case Je(label)         => s"  je    ${label.name}"
      case Jl(label)         => s"  jl    ${label.name}"
      case Jo(label)         => s"  jo    ${label.name}"
      case Jne(label)        => s"  jne   ${label.name}"
      case Idiv(op1)         => s"  idiv${instructionPostfix(op1)} ${this(op1)}"
    }
  }

  override def apply(reg: Reg): String =
    reg match {
      case Rax => "%rax"
      case Rbx => "%rbx"
      case Rcx => "%rcx"
      case Rdx => "%rdx"
      case Rbp => "%rbp"
      case Rsp => "%rsp"
      case Rsi => "%rsi"
      case Rdi => "%rdi"
      case R8  => "%r8"
      case R9  => "%r9"
      case R10 => "%r10"
      case R11 => "%r11"
      case R12 => "%r12"
      case R13 => "%r13"
      case R14 => "%r14"
      case R15 => "%r15"
    }

  override def apply(location: Location): String = location match {
    case reg: Reg => this(reg)
    case Address(offset, base, index, scale) =>
      s"${memFormat(offset)}(${memFormat(base)}, ${memFormat(index)}, ${memFormat(scale)})"
    case Immediate(value) => s"$$$value"
    case Label(name)      => name
  }

  private def memFormat(mem: MemOp): String = mem match {
    case reg: Reg         => this(reg)
    case Immediate(value) => s"$value"
    case Label(name)      => name
  }

  private def instructionPostfix(location: Location): String = location match {
    case r: Reg =>
      r.size match {
        case Size1  => "b"
        case Size8  => "b"
        case Size16 => "w"
        case Size32 => "l"
        case Size64 => "q"
      }
    case _ => ???
  }
}
