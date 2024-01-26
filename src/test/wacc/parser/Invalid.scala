import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks
import parsley.Failure
import src.main.wacc.parser

import java.io.File

class Invalid extends AnyFlatSpec with TableDrivenPropertyChecks {

  private val testDir = "src/test/test_files/invalid/syntaxErr"

  private def listFiles(directory: File): Seq[File] = {
    if (directory.isDirectory) {
      val files = directory.listFiles
      val subdirectories = files.filter(_.isDirectory)
      val filesInSubdirectories = subdirectories.flatMap(listFiles)
      files.filter(_.isFile).toIndexedSeq ++ filesInSubdirectories
    } else {
      Seq.empty
    }
  }

  val files = listFiles(new File(testDir))
  val cases = Table("cases", files: _*)

  forAll(cases) { file =>
    s"${file.getParentFile}/${file.getName}" should "fail to parse" in {
      val source = scala.io.Source.fromFile(file)
      val lines =
        try source.getLines().mkString("\n")
        finally source.close()
      parser.parse(lines) shouldBe a[Failure[_]]
    }
  }
}
