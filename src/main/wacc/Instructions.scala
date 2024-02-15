package src.main.wacc

sealed trait Instruction

sealed trait Location // Change this name ????
sealed trait Dest extends Location
sealed trait Operand extends Location
sealed trait MemOp extends Location

sealed trait Reg extends Dest with Operand with MemOp

// Registers
case object Rax extends Reg
case object Rbx extends Reg
case object Rcx extends Reg
case object Rdx extends Reg
case object Rbp extends Reg
case object Rsp extends Reg
case object Rsi extends Reg
case object Rdi extends Reg

case object R8 extends Reg
case object R9 extends Reg
case object R10 extends Reg
case object R11 extends Reg
case object R12 extends Reg
case object R13 extends Reg
case object R14 extends Reg
case object R15 extends Reg

final case class Address(
  offset: MemOp = Immediate(0),
  base: MemOp,
  index: MemOp = Immediate(0),
  scale: MemOp = Immediate(1)) extends Dest with Operand
final case class Immediate(value: Long) extends Operand with MemOp

case object Ret extends Instruction
case object Cltd extends Instruction

final case class Directive(name: String) extends Instruction
final case class Label(name: String) extends Instruction with MemOp
final case class Mov(op: Operand, dest: Dest) extends Instruction
final case class Pop(dest: Dest) extends Instruction
final case class Push(op: Operand) extends Instruction
final case class CallAsm(label: Label) extends Instruction
final case class AndAsm(op: Operand, dest: Dest) extends Instruction
final case class Setne(dest: Dest) extends Instruction
final case class Lea(op: Address, dest: Dest) extends Instruction

final case class AddAsm(op: Operand, dest: Reg) extends Instruction
final case class SubAsm(op: Operand, dest: Reg) extends Instruction
final case class Cmp(op1: Operand, op2: Operand) extends Instruction

final case class Jmp(label: Label) extends Instruction
final case class Je(label: Label) extends Instruction
final case class Jl(label: Label) extends Instruction
final case class Jo(label: Label) extends Instruction
final case class Jne(label: Label) extends Instruction
final case class Idiv(op: Operand) extends Instruction

trait Formatter {
  def apply(cfgNode: CfgNode): String = this(cfgNode.instruction)
  def apply(instruction: Instruction): String
  def apply(reg: Reg): String
  def apply(location: Location): String
}

// x86-64 AT&T instructions
object x86Formatter extends Formatter {
  override def apply(instruction: Instruction): String =
    instruction match {
      case Directive(name)  => s".$name"
      case Label(name)       => s"$name:"
      case Ret               => "  ret"
      case Cltd              => "  cltd"
      case Mov(op1, dest)    => s"  movq  ${this(op1)}, ${this(dest)}"
      case Pop(dest)         => s"  popq  ${this(dest)}"
      case Push(op1)         => s"  pushq ${this(op1)}"
      case CallAsm(label)    => s"  call  ${label.name}"
      case AndAsm(op1, dest) => s"  and   ${this(op1)}, ${this(dest)}"
      case Setne(dest)       => s"  setne ${this(dest)}"
      case Lea(op1, dest)    => s"  leaq  ${this(op1)}, ${this(dest)}"
      case AddAsm(op1, dest) => s"  addq  ${this(op1)}, ${this(dest)}"
      case SubAsm(op1, dest) => s"  subq  ${this(op1)}, ${this(dest)}"
      case Cmp(op1, op2)     => s"  cmpq  ${this(op1)}, ${this(op2)}"
      case Jmp(label)        => s"  jmp   ${label.name}"
      case Je(label)         => s"  je    ${label.name}"
      case Jl(label)         => s"  jl    ${label.name}"
      case Jo(label)         => s"  jo    ${label.name}"
      case Jne(label)        => s"  jne   ${label.name}"
      case Idiv(op1)         => s"  idivq ${this(op1)}"
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
    case reg: Reg => this(reg)
    case Immediate(value) => s"$value"
    case Label(name) => name
  }
}
