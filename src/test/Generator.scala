package src.test

import src.main.wacc._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks
import scala.sys.process._
import scala.io.Source
import java.io.{File, FileOutputStream, PrintStream}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

sealed trait Err
case object CompilationError extends Err

class Generator extends AnyFlatSpec with TableDrivenPropertyChecks {

  behavior of "generator"
  forAll(Table("cases", TestFiles("valid/"): _*)) { file =>
    it should s"produce the correct output and exit code for ${file.getParentFile}/${file.getName}" in {
      val filePath = file.getPath
      val filename = file.getName.replaceFirst("\\.wacc$", "")
      val nullPrintStream = new PrintStream(new FileOutputStream("/dev/null"))
      Console.withOut(nullPrintStream) {
        Main.main(Array(filePath))
      }
      compileAssembly(s"$filename.s") shouldEqual None
      val (expOut, expExit) = parseWaccFile(filePath)
      try {
        val (out, exit) = Await.result(Future(runBinary(filename)), 10.seconds)
        out shouldEqual expOut
        exit shouldEqual expExit
      } catch {
        case e: Exception =>
          deleteFile(filename)
          fail(s"Error running binary: ${e.getMessage}")
      }
    }
  }

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
        output = lines.takeWhile(!_.isBlank).map(_.drop(2)).mkString("\n")
      } else if (line.startsWith("# Exit:")) {
        exitCode = lines.next().drop(2).toInt
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
