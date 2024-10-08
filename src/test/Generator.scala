package src.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks
import src.main.wacc._

import java.io.{File, FileOutputStream, PrintStream}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.sys.process._

sealed trait Err
case object CompilationError extends Err

class Generator extends AnyFlatSpec with TableDrivenPropertyChecks {

  behavior of "generator"
  forAll(Table("cases", TestFiles("valid/"): _*)) { file =>
    if (file.getParentFile.getName != "advanced") {
      it should s"produce the correct output and exit code for ${file.getParentFile}/${file.getName}" in {
        val filePath = file.getPath
        val filename = file.getName.replaceFirst("\\.wacc$", "")
        Console.withOut(new PrintStream(new FileOutputStream("/dev/null"))) {
          Main.main(Array(filePath))
        }
        testFileMaker.compileAssembly(s"$filename.s") shouldEqual None
        val (input, expOut, expExit) = testFileMaker.parseWaccFile(filePath)
        var ret = ("", 1)
        try {
          ret = Await.result(Future(testFileMaker.runBinary(filename, input)), 60.seconds)
        } catch {
          case e: Exception =>
            testFileMaker.deleteFile(filename)
            fail(s"Error running binary: ${e.getMessage}")
        }
        val (out, exit) = ret
        withClue(s"Output: $out\nShould have matched: $expOut\n") {
          out.matches(expOut) shouldBe true
        }
        exit shouldEqual expExit
      }
    }
  }
}

object testFileMaker {
  def compileAssembly(assemblyFile: String): Option[Err] = {
    try {
      val binaryFile = assemblyFile.replaceFirst("\\.s$", "")
      val compileCommand = s"gcc $assemblyFile -o $binaryFile"
      val compileResult = compileCommand.!
      if (compileResult != 0) {
        return Some(CompilationError)
      }
      None
    } finally {
      deleteFile(assemblyFile)
    }
  }

  def runBinary(binaryFile: String, input: String): (String, Int) = {
    val process = s"echo $input" #> s"./$binaryFile"
    new File(binaryFile).setExecutable(true)
    val output = new StringBuilder
    val exitCode = process ! ProcessLogger(output.append(_))
    deleteFile(binaryFile)
    (output.toString, exitCode)
  }

  def sanitiseRegex(s: String): String = s
    .replaceAll("\\\\", "\\\\\\\\")
    .replaceAll("\\{", "\\\\{")
    .replaceAll("}", "\\\\}")
    .replaceAll("\\+", "\\\\+")
    .replaceAll("\\?", "\\\\?")
    .replaceAll("\\*", "\\\\*")
    .replaceAll("\\(", "\\\\(")
    .replaceAll("\\)", "\\\\)")
    .replaceAll("\\[", "\\\\[")
    .replaceAll("]", "\\\\]")
    .replaceAll("\\^", "\\\\^")
    .replace("$", "\\$")
    .replaceAll("\\.", "\\\\.")
    .replaceAll("\\|", "\\\\|")
    .replaceAll("-", "\\\\-")

  def parseWaccFile(filePath: String): (String, String, Int) = {
    var output: String = ""
    var input: String = ""
    var exitCode: Int = 0
    val source = Source.fromFile(filePath)
    val lines = source.getLines()

    for (line <- lines) {
      if (line.startsWith("# Output:")) {
        output = sanitiseRegex(lines.takeWhile(!_.isBlank).map(_.drop(2))
          .mkString("")).replaceAll("#.*?#", ".*?")
      } else if (line.startsWith("# Exit:")) {
        exitCode = lines.next().drop(2).toInt
      } else if (line.startsWith("# Input:")) {
        input = line.drop(8) ++ lines.takeWhile(!_.isBlank).mkString("")
      }
    }

    source.close()
    (input, output, exitCode)
  }

  def deleteFile(filePath: String): Unit = {
    val file = new File(filePath)
    if (file.exists()) {
      file.delete()
    }
  }
}
