sealed trait Instruction
sealed trait Dest
sealed trait Operand

sealed trait Reg
case object rax extends Reg
case object rbx extends Reg
case object rcx extends Reg
case object rdx extends Reg
case object rbp extends Reg
case object rsp extends Reg
case object rsi extends Reg
case object rdi extends Reg

case object r8 extends Reg
case object r9 extends Reg
case object r10 extends Reg
case object r11 extends Reg
case object r12 extends Reg
case object r13 extends Reg
case object r14 extends Reg
case object r15 extends Reg

case class Register(register: Reg) extends Dest with Operand
case class Address(value: Int) extends Dest with Operand

case class Immediate(value: Int) extends Operand

case class Label(name: String) extends Instruction
case class Add(dest: Register, op1: Operand, op2: Operand) extends Instruction
