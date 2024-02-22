package src.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks
import src.main.wacc.{generator, analyser, parser, x86Formatter}

import scala.sys.process._
import scala.io.Source
import java.io.File

class Generator extends AnyFlatSpec with TableDrivenPropertyChecks {

  behavior of "generator"
  forAll(Table("cases", TestFiles("valid/"): _*)) { file =>
    val fileName = removeFileExtension(s"${file.getParentFile}/${file.getName}")
    val binaryFile = compileAssembly(generator.generate((parser.parse(TestFiles.getLines(file)).get), x86Formatter), s"${fileName}.o")
    val (binOutput, binExitCode) = runBinary(s"${fileName}.s")
    val (waccOutput, waccExitCode) = parseWaccFile(s"${fileName}.wacc")
    (binOutput) should equal (waccOutput)
    (binExitCode) should equal (waccExitCode)
    deleteFile(s"${fileName}.s")
  }

  def compileAssembly(assemblyFile: String, outputFile: String): Int = {
    val command = s"as -o $outputFile $assemblyFile"
    command.!
  }

  def runBinary(binaryFile: String): (String, Int) = {
    val process = Process(binaryFile)
    val output = new StringBuilder
    val exitCode = process ! ProcessLogger(output.append(_))

    (output.toString, exitCode)
  }

  def parseWaccFile(filePath: String): (String, Int) = {
    var output: String = ""
    var exitCode: Int = 0
    val source = Source.fromFile(filePath)
    val lines = source.getLines()

    for (line <- lines) {
      if (line.startsWith("# Output:")){
        output = lines.takeWhile(!_.startsWith("#")).mkString("\n").trim()
      } else if (line.startsWith("# Output:")){
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

  def removeFileExtension(fileName: String): String = {
    if (fileName.contains(".")) {
      fileName.substring(0, fileName.lastIndexOf('.'))
    } else {
      fileName
    }
  }
}

/*

object AssemblerCompiler extends App {
  def apply(filepath: String): Unit {
    val assemblyFile = generator.generate((parser.parse(TestFiles.getLines(file)).get), x86Formatter)
    val objectFile = s"${file.getParentFile}/${file.getName}.o"
    val executableName = s"${file.getParentFile}/${file.getName}"

    s"as -o $objectFile $assemblyFile".!

    s"Id -o $executableName $objectFile".!

    println(s"Compilication successful. Executable: $executableName") 
  }
}

object RunExeutable extends App {
  val executableName = s"${file.getParentFile}/${file.getName}"

  val processBuilder: ProcessBuilder = Process(executableName)
  val process: Process = processBuilder.run()

  val exitCode: Int = process.exitValue()

  val output: String = process.!!

  println(s"Exit code: $exitCode")
  println(s"Output: $output")

}

object ParseOutput {
  def main(args: Array[String]): Unit = {
    val cmd = s"scala ${file.getParentFile}/${file.getName}"
    val outputAndStatus = cmd !

    val exitCodePattern = """# Exit code: (\d+)""".r
    val exitCode = exitCodePattern.findFirstMatchIn(outputAndStatus).map(_.group(1).toInt)

    val outputPattern = """#Output: """
    val output = outputPattern.findFirstMatchIn(outputAndStatus).isDefined

    println(s"Output: $output")
    println(s"Exit code: $exitCode")

  }
}
*/