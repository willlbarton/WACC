package src.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks
import src.main.wacc.{generator, analyser, parser, x86Formatter}

import scala.sys.process._
import scala.io.Source
import java.io.File
import java.io.{BufferedWriter, FileWriter}
import java.util.Optional

sealed trait Err
case object CompilationError extends Err

class Generator extends AnyFlatSpec with TableDrivenPropertyChecks {

  behavior of "generator"
  forAll(Table("cases", TestFiles("valid/basic/"): _*)) { file =>
    it should s"produce the correct output and exit code for ${file.getParentFile}/${file.getName}" in {
      val fileName = file.getPath()
      val asmFileName = waccToAsm(fileName)
      val binaryFile = compileAssembly(asmFileName)
      binaryFile match {
        case None => {
          val binaryFile = asmFileName.replaceFirst("\\.s$", "")
          val (binOutput, binExitCode) = runBinary(binaryFile)
          deleteFile(binaryFile)
          val (waccOutput, waccExitCode) = parseWaccFile(s"${fileName}")
          (binOutput) should equal(waccOutput)
          (binExitCode) should equal(waccExitCode)
        }
        case _ => fail("Compilation error")
      }
    }
  }

  def waccToAsm(waccFile: String): String = {
    val source = scala.io.Source.fromFile(waccFile)
    val program =
      try source.getLines().mkString("\n")
      finally source.close()
    val asmFileName = waccFile.split("/").last.replaceFirst("\\.\\w+$", ".s")
    val parsed = parser.parse(program).get
    val asmString = generator.generate(parsed, x86Formatter)
    val asmFile = new File(asmFileName)
    val writer = new BufferedWriter(
      new FileWriter(asmFileName)
    )
    writer.write(asmString)
    asmFileName
  }

  def compileAssembly(assemblyFile: String): Option[Err] = {
    val binaryFile = assemblyFile.replaceFirst("\\.s$", "")
    val compileCommand = s"gcc -c $assemblyFile -o $binaryFile"
    val compileResult = compileCommand.!
    deleteFile(assemblyFile)
    if (compileResult != 0) {
      return Some(CompilationError)
    }
    None
  }

  def runBinary(binaryFile: String): (String, Int) = {
    val process = Process(s"./$binaryFile")
    val binFile = new File(binaryFile)
    binFile.setExecutable(true)
    val output = new StringBuilder
    val exitCode = process ! ProcessLogger(output.append(_))
    deleteFile(binaryFile)
    (output.toString, exitCode)
  }

  def parseWaccFile(filePath: String): (String, Int) = {
    var output: String = ""
    var exitCode: Int = 0
    val source = Source.fromFile(filePath)
    val lines = source.getLines()

    for (line <- lines) {
      if (line.startsWith("# Output:")) {
        output = lines.takeWhile(!_.startsWith("#")).mkString("\n").trim()
      } else if (line.startsWith("# Exit:")) {
        exitCode = lines.takeWhile(!_.startsWith("#")).mkString("\n").trim().toInt
      }
    }

    source.close()
    (output, exitCode)
  }

  def deleteFile(filePath: String): Unit = {
    val file = new File(filePath)
    if (file.exists()) {
      file.delete();
    }
  }
}

// object Generator {
//   def main(args: Array[String]): Unit = {
//     val testFile = "src/test/test_files/valid/basic/skip/justSkip.wacc"
//     val generator = new Generator()
//     val (output, exitCode) = generator.parseWaccFile(testFile)
//     val asmFile = generator.waccToAsm(testFile)

//     val input = generator.compileAssembly(asmFile)
//     val (binOut, binEx) = generator.runBinary("justSkip")
//     println(binOut)
//     println(s"Output: $output")
//     println(s"Exit code: $exitCode")
//   }
// }