package src.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks
import parsley.{Failure, Success}
import src.main.wacc.parser

class Parser extends AnyFlatSpec with TableDrivenPropertyChecks {

  behavior of "parser-valid"
  forAll(Table("cases", TestFiles("valid/"): _*)) { file =>
    it should s"successfully parse ${file.getParentFile}/${file.getName}" in {
      parser.parse(TestFiles.getLines(file)) shouldBe a [Success[_]]
    }
  }

  behavior of "parser-invalid"
  forAll(Table("cases", TestFiles("invalid/syntaxErr/"): _*)) { file =>
    it should s"fail to parse ${file.getParentFile}/${file.getName}" in {
      parser.parse(TestFiles.getLines(file)) shouldBe a [Failure[_]]
    }
  }

}
