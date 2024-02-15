package src.main.wacc

object generator {

  def generate(program: Program, formatter: Formatter): String = genProgram(program).toList
    .map(formatter(_)).mkString("\n")

  private def genProgram(program: Program): ControlFlowGraph = ???
}