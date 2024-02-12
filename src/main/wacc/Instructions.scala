package src.main.wacc.Instructions

sealed trait Instruction
sealed trait Dest
sealed trait Operand

sealed trait Reg

// base registers
case object rax extends Reg {
  override def toString: String = "%rax"
}

case object rbx extends Reg {
  override def toString: String = "%rbx"
}

case object rcx extends Reg {
  override def toString: String = "%rcx"
}

case object rdx extends Reg {
  override def toString: String = "%rdx"
}

case object rbp extends Reg {
  override def toString: String = "%rbp"
}

case object rsp extends Reg {
  override def toString: String = "%rsp"
}

case object rsi extends Reg {
  override def toString: String = "%rsi"
}

case object rdi extends Reg {
  override def toString: String = "%rdi"
}

case object r8 extends Reg {
  override def toString: String = "%r8"
}

case object r9 extends Reg {
  override def toString: String = "%r9"
}

case object r10 extends Reg {
  override def toString: String = "%r10"
}

case object r11 extends Reg {
  override def toString: String = "%r11"
}

case object r12 extends Reg {
  override def toString: String = "%r12"
}

case object r13 extends Reg {
  override def toString: String = "%r13"
}

case object r14 extends Reg {
  override def toString: String = "%r14"
}

case object r15 extends Reg {
  override def toString: String = "%r15"
}

case class Register(register: Reg) extends Dest with Operand {
  override def toString: String = register.toString
}

case class Address(value: Int) extends Dest with Operand {
  override def toString: String = s"$value(%rsp)" // idk if this is right
}

// x86-64 AT&T instructions
case object Ret extends Instruction {
  override def toString: String = "ret"
}

case object Cltd extends Instruction {
  override def toString: String = "cltd"
}

case class Label(name: String) extends Instruction {
  override def toString: String = s"$name:"
}

case class Mov(op1: Operand, dest: Dest) extends Instruction {
  override def toString: String = s"movq $op1, $dest"
}

case class Pop(dest: Dest) extends Instruction {
  override def toString: String = s"popq $dest"
}

case class Push(op1: Operand) extends Instruction {
  override def toString: String = s"pushq $op1"
}

case class Call(label: Label) extends Instruction {
  override def toString: String = s"call $label"
}

case class And(op1: Operand, dest: Dest) extends Instruction {
  override def toString: String = s"and $op1, $dest"
}

case class Setne(dest: Dest) extends Instruction {
  override def toString: String = s"setne $dest"
}

case class Cmovge(op1: Operand, dest: Dest) extends Instruction {
  override def toString: String = s"cmovge $op1, $dest"
}

case class Movsl(op1: Operand, dest: Dest) extends Instruction {
  override def toString: String = s"movslq $op1, $dest"
}

case class Add(op1: Operand, dest: Register) extends Instruction {
  override def toString: String = s"addq $op1, $dest"
}

case class Sub(op1: Operand, dest: Register) extends Instruction {
  override def toString: String = s"subq $op1, $dest"
}

case class Cmp(op1: Operand, op2: Operand) extends Instruction {
  override def toString: String = s"cmpq $op1, $op2"
}

case class Jmp(label: Label) extends Instruction {
  override def toString: String = s"jmp ${label.name}"
}

case class Je(label: Label) extends Instruction {
  override def toString: String = s"je ${label.name}"
}

case class Jl(label: Label) extends Instruction {
  override def toString: String = s"jl ${label.name}"
}

case class Jo(label: Label) extends Instruction {
  override def toString: String = s"jo ${label.name}"
}

case class Jne(label: Label) extends Instruction {
  override def toString: String = s"jne ${label.name}"
}

case class Idiv(op1: Operand) extends Instruction {
  override def toString: String = s"idivq $op1"
}
