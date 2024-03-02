import org.scalatest.funsuite.AnyFunSuite
import src.main.wacc._
import scala.collection.mutable.ListBuffer
import src.main.wacc.builtInFunctions._

class BackEndUnit extends AnyFunSuite {
  private lazy val indent = "        "

  val immValue: Int = 42
  val immOperand: Imm = Imm(immValue)
  val eaxReg: Eax = Eax()
  val rbpReg: Reg = Rbp
  val label: Label = Label("myLabel")

  test("Imm should convert int to immediate correctly") {
    assert(immValue === immOperand.value)
  }

  test("Formatting immediate should produce correct string") {
    assert(x86Formatter.apply(immOperand) === s"$$$immValue")
  }

  test("Formatting instruction should produce correct string") {
    val movInstruction: Instruction = Mov(immOperand, eaxReg)
    assert(x86Formatter.apply(movInstruction) === s"        movl  $$$immValue, %eax")
  }

  test("Formatting program should produce correct string") {
    val prog: ListBuffer[Instruction] = ListBuffer(
      Mov(Imm(1), Eax()),
      Push(Eax()),
      Pop(Eax()),
      Ret
    )

    val formattedProgram = formatter.format(prog, x86Formatter)
    val expectedOutput =
      """        movl  $1, %eax
        |        pushl %eax
        |        popl  %eax
        |        ret""".stripMargin

    assert(formattedProgram.trim === expectedOutput.trim)
  }

  test("Formatting jump instructions should produce correct strings") {
    val jmpInstruction: Instruction = Jmp(label)
    val joInstruction: Instruction = Jo(label)

    assert(x86Formatter.apply(jmpInstruction) === s"        jmp   ${label.name}")
    assert(x86Formatter.apply(joInstruction) === s"        jo    ${label.name}")

  }

  test("Formatting arithmetic instructions should produce correct strings") {
    val addInstruction: Instruction = AddAsm(Imm(2), eaxReg)
    val subInstruction: Instruction = SubAsm(Imm(1), eaxReg)

    assert(x86Formatter.apply(addInstruction) === s"        addl  $$2, %eax")
    assert(x86Formatter.apply(subInstruction) === s"        subl  $$1, %eax")
  }

  test("Formatting conditional move instructions should produce correct strings") {
    val cmovlInstruction: Instruction = CMovl(Imm(0), eaxReg)
    val cmovgeInstruction: Instruction = CMovge(Imm(1), eaxReg)
    val cmovneInstruction: Instruction = CMovne(Imm(2), eaxReg)

    assert(x86Formatter.apply(cmovlInstruction) === s"        cmovl $$0, %eax")
    assert(x86Formatter.apply(cmovgeInstruction) === s"        cmovge $$1, %eax")
    assert(x86Formatter.apply(cmovneInstruction) === s"        cmovne $$2, %eax")
  }

  test("lb should handle single instructions") {
    val instruction: Instruction = Mov(Imm(42), Eax())
    val resultBuffer = lb(instruction)
    assert(resultBuffer.length === 1)
    assert(resultBuffer.head === instruction)
  }

  test("lb should handle multiple instructions") {
    val instruction1: Instruction = Mov(Imm(42), Eax())
    val instruction2: Instruction = AddAsm(Imm(1), Eax())
    val resultBuffer = lb(instruction1, instruction2)
    assert(resultBuffer.length === 2)
    assert(resultBuffer.head === instruction1)
    assert(resultBuffer(1) === instruction2)
  }

  test("lb should handle nested IterableOnce") {
    val instruction1: Instruction = Mov(Imm(42), Eax())
    val instruction2: Instruction = AddAsm(Imm(1), Eax())
    val nestedIterable: IterableOnce[Instruction] = List(instruction1, instruction2)
    val resultBuffer = lb(nestedIterable)
    assert(resultBuffer.length === 2)
    assert(resultBuffer.head === instruction1)
    assert(resultBuffer(1) === instruction2)
  }

  test("lb should throw an IllegalArgumentException for unsupported types within IterableOnce") {
    val instruction1: Instruction = Mov(Imm(42), Eax())
    val unsupportedType: String = "This is not an instruction"
    val nestedIterable: IterableOnce[Any] = List(instruction1, unsupportedType)
    assertThrows[IllegalArgumentException] {
      lb(nestedIterable)
    }
  }
}
