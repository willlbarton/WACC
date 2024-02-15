package src.main.wacc.Instructions

sealed trait Instruction

sealed trait Location // Change this name ????
sealed trait Dest extends Location
sealed trait Operand extends Location

sealed trait Reg

// base registers
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

case class Register(register: Reg) extends Dest with Operand
case class Address(value: Int) extends Dest with Operand

// x86-64 AT&T instructions
case object Ret extends Instruction
case object Cltd extends Instruction

case class Label(name: String) extends Instruction
case class Mov(op1: Operand, dest: Dest) extends Instruction
case class Pop(dest: Dest) extends Instruction
case class Push(op1: Operand) extends Instruction
case class Call(label: Label) extends Instruction
case class And(op1: Operand, dest: Dest) extends Instruction
case class Setne(dest: Dest) extends Instruction
case class Cmovge(op1: Operand, dest: Dest) extends Instruction
case class Movsl(op1: Operand, dest: Dest) extends Instruction

case class Add(op1: Operand, dest: Register) extends Instruction
case class Sub(op1: Operand, dest: Register) extends Instruction
case class Cmp(op1: Operand, op2: Operand) extends Instruction

case class Jmp(label: Label) extends Instruction
case class Je(label: Label) extends Instruction
case class Jl(label: Label) extends Instruction
case class Jo(label: Label) extends Instruction
case class Jne(label: Label) extends Instruction
case class Idiv(op1: Operand) extends Instruction

object Formatter {
  def apply(instruction: Instruction): String =
    instruction match {
      case Ret               => "ret"
      case Cltd              => "cltd"
      case Label(name)       => s"$name:"
      case Mov(op1, dest)    => s"movq ${apply(op1)}, ${apply(dest)}"
      case Pop(dest)         => s"popq ${apply(dest)}"
      case Push(op1)         => s"pushq ${apply(op1)}"
      case Call(label)       => s"call ${apply(label)}"
      case And(op1, dest)    => s"and ${apply(op1)}, ${apply(dest)})"
      case Setne(dest)       => s"setne ${apply(dest)}"
      case Cmovge(op1, dest) => s"cmovge ${apply(op1)}, ${apply(dest)}"
      case Movsl(op1, dest)  => s"movslq ${apply(op1)}, ${apply(dest)}"
      case Add(op1, dest)    => s"addq ${apply(op1)}, ${apply(dest)}"
      case Sub(op1, dest)    => s"subq ${apply(op1)}, ${apply(dest)}"
      case Cmp(op1, op2)     => s"cmpq ${apply(op1)}, ${apply(op2)}"
      case Jmp(label)        => s"jmp ${label.name}"
      case Je(label)         => s"je ${label.name}"
      case Jl(label)         => s"jl ${label.name}"
      case Jo(label)         => s"jo ${label.name}"
      case Jne(label)        => s"jne ${label.name}"
      case Idiv(op1)         => s"idivq ${apply(op1)}"
    }

  def apply(reg: Reg): String =
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

  def apply(location: Location): String = location match {
    case Register(reg)  => apply(reg)
    case Address(value) => s"$value(%rsp)" // idk if this is right
  }
}
