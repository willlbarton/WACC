package src.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks
import src.main.wacc._

import java.io.{FileOutputStream, PrintStream}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class Optimiser extends AnyFlatSpec with TableDrivenPropertyChecks {

  behavior of "optimiser"
  forAll(Table("cases", TestFiles("valid/"): _*)) { file =>
    if (file.getParentFile.getName != "advanced") {
      it should s"produce the correct output and exit code for ${file.getParentFile}/${file.getName}" in {
        val filePath = file.getPath
        val filename = file.getName.replaceFirst("\\.wacc$", "")
        Console.withOut(new PrintStream(new FileOutputStream("/dev/null"))) {
          Main.main(Array(filePath, "-o"))
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