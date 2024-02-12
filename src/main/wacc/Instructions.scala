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

// x86-64 instructions
case object Ret extends Instruction {
  override def toString: String = "ret"
}

case class Label(name: String) extends Instruction {
  override def toString: String = s"$name:"
}

case class Mov(src: Operand, dest: Dest) extends Instruction {
  override def toString: String = s"movq $src, $dest"
}

case class Pop(dest: Dest) extends Instruction {
  override def toString: String = s"popq $dest"
}

case class Push(src: Operand) extends Instruction {
  override def toString: String = s"pushq $src"
}

case class Call(label: Label) extends Instruction {
  override def toString: String = s"call $label"
}

case class And(src: Operand, dest: Dest) extends Instruction {
  override def toString: String = s"and $src, $dest"
}

case class Setne(dest: Dest) extends Instruction {
  override def toString: String = s"setne $dest"
}

case class Cmovge(src: Operand, dest: Dest) extends Instruction {
  override def toString: String = s"cmovge $src, $dest"
}
