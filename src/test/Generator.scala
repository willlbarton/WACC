package src.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks
import src.main.wacc.{generator}

class Generator extends AnyFlatSpec with TableDrivenPropertyChecks {

  behavior of "generator-valid"
  forAll(Table("cases", TestFiles("valid/"): _*)) { file =>
    it should s"not produce errors for ${file.getParentFile}/${file.getName}" in {
      generator.generate(analyser.analyse(parser.parse(TestFiles.getLines(file)).get)) should not be Exit 
    }
  }

  behavior of "generator-invalid"
  forAll(Table("cases", TestFiles("invalid/"): _*)) { file =>
    it should s"produce errors for ${file.getParentFile}/${file.getName}" in {
      generator.generate(analyser.analyse(parser.parse(TestFiles.getLines(file)).get)) shouldBe Exit
    }
  }

}
