package src.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks
import src.main.wacc._

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
      val filePath = file.getPath
      val filename = file.getName.replaceFirst("\\.wacc$", "")
      Main.main(Array(filePath))
      compileAssembly(s"$filename.s") shouldEqual None
      val (expOut, expExit) = parseWaccFile(filePath)
      val (out, exit) = runBinary(filename)
      out shouldEqual expOut
      exit shouldEqual expExit
    }
  }

  def compileAssembly(assemblyFile: String): Option[Err] = {
    val binaryFile = assemblyFile.replaceFirst("\\.s$", "")
    val compileCommand = s"gcc $assemblyFile -o $binaryFile"
    val compileResult = compileCommand.!
    deleteFile(assemblyFile)
    if (compileResult != 0) {
      return Some(CompilationError)
    }
    None
  }

  def runBinary(binaryFile: String): (String, Int) = {
    val process = Process(s"./$binaryFile")
    new File(binaryFile).setExecutable(true)
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
        output = lines.takeWhile(!_.equals("#")).map(_.drop(2)).mkString("\n").trim()
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
      file.delete()
    }
  }
}

object Generator {
 def main(args: Array[String]): Unit = {
   val filePath = "../src/test/test_files/valid/IO/print/println.wacc"
   val generator = new Generator()

   Main.main(Array(filePath))
   generator.compileAssembly(s"println.s") shouldEqual None
   val (out, exit) = generator.runBinary("println")
   println(s"Output: $out")
   println(s"Exit code: $exit")
 }
}