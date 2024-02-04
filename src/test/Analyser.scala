package src.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks
import src.main.wacc.{analyser, parser}

class Analyser extends AnyFlatSpec with TableDrivenPropertyChecks {

  behavior of "analyser-valid"
  forAll(Table("cases", TestFiles("valid/"): _*)) { file =>
    it should s"not produce errors for ${file.getParentFile}/${file.getName}" in {
      analyser.analyse(parser.parse(TestFiles.getLines(file)).get) shouldBe empty
    }
  }

  behavior of "analyser-invalid"
  forAll(Table("analyser", TestFiles("invalid/semanticErr/"): _*)) { file =>
    it should s"produce errors for ${file.getParentFile}/${file.getName}" in {
      analyser.analyse(parser.parse(TestFiles.getLines(file)).get) should not be empty
    }
  }

}
