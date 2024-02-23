package src.test

import java.io.PrintWriter
import scala.sys.process._
import scala.io.Source
import scala.util.matching.Regex
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks
import src.main.wacc.{parser, analyser, generator}
import java.util.Formatter
import src.main.wacc.x86Formatter
import java.io.File
import java.nio.file.{Paths, Files}
import src.main.wacc._
import java.{util => ju}

sealed trait Err
case object CompilationError extends Err
case object ExecutionError extends Err

class Generator extends AnyFlatSpec with TableDrivenPropertyChecks {

  behavior of "generator"
  forAll(Table("cases", TestFiles("valid/"): _*)) { file =>
    it should s"produce the correct output and exit code for ${file.getParentFile}/${file.getName}" in {
      val asmFile = buildAsm(file.getPath())
      val (output, exitCode) = regex(file.getPath())
      val result = assembleAndRun(asmFile)
      result match {
        case Left(CompilationError) => fail("Compilation error")
        case Left(ExecutionError)   => fail("Execution error")
        case Right((out, code)) =>
          out shouldBe output
          code shouldBe exitCode
      }
    }
  }

  def buildAsm(testFile: String): String = {
    val fileName = testFile.split("/").last
    val outputFile = fileName.replaceFirst("\\.\\w+$", ".s")
    val buildCommand = s"scala-cli run . -nowarn -q -- $testFile"
    buildCommand.!
    outputFile
  }

  def assembleAndRun(testFile: String): Either[Err, (String, Int)] = {
    val binaryFile = "output"
    val compileCommand = s"gcc $testFile -o $binaryFile"
    val compileResult = compileCommand.!
    Files.delete(Paths.get(testFile))
    if (compileResult != 0) {
      return Left(CompilationError)
    }
    val process = Process(s"./$binaryFile")
    val output = process.lazyLines_!.mkString(",")
    val exitCode = process.!
    Files.delete(Paths.get(binaryFile))
    Right((output, exitCode))
  }

  def regex(testFile: String): (String, Int) = {
    val source = Source.fromFile(testFile)
    val fileString = source.getLines().mkString("\n");
    val exitCodeRegex: Regex = """Exit:\s*(# \d+)""".r
    val outputRegex: Regex = """Output:\s((# .*\n)*)""".r
    val exitCode = exitCodeRegex.findFirstMatchIn(fileString).map(_.group(1)) match {
      case Some(value) => value.replace("#", "").trim().toInt
      case None        => 0
    }
    val output = outputRegex.findFirstMatchIn(fileString).map(_.group(1)) match {
      case Some(value) =>
        value
          .split("\n")
          .map {
            case str if str.startsWith("#") => str.substring(1)
            case x                          => x
          }
          .map(_.trim())
          .mkString(",")
      case None => ""
    }
    (output, exitCode)
  }

}

object Generator {
  def main(args: Array[String]): Unit = {
    val testFile = "src/test/test_files/valid/array/printRef.wacc"
    val generator = new Generator()
    val (output, exitCode) = generator.regex(testFile)

    val input = generator.buildAsm(testFile)

    val result = generator.assembleAndRun(input) match {
      case Left(value)        => (List(), 0)
      case Right((out, code)) => (out, code)
    }
    println(s"Output: $output")
    println(s"Exit code: $exitCode")
    println(result)
  }
}
