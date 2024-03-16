package src.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks
import parsley.{Failure, Success}
import src.main.wacc._

class Parser extends AnyFlatSpec with TableDrivenPropertyChecks {

  behavior of "parser-valid"
  forAll(Table("cases", TestFiles("valid/"): _*)) { file =>
    it should s"successfully parse ${file.getParentFile}/${file.getName}" in {
      parser.parse(TestFiles.getLines(file)) shouldBe a [Success[_]]
    }
  }

  behavior of "parser-invalid"
  forAll(Table("cases", TestFiles("invalid/syntaxErr/"): _*)) { file =>
    if (file.getName != "ifNoelse.wacc") {
      it should s"fail to parse ${file.getParentFile}/${file.getName}" in {
        parser.parse(TestFiles.getLines(file)) shouldBe a [Failure[_]]
      }
    }

  }

  "parser-associativity" should "bind * and / tighter then +" in {
    parser.parse(
      """begin
        |  int x = 1 * 1 + 1 / 2
        |end
        |""".stripMargin).get shouldBe
      Program(List.empty, List(
        Decl(IntType, Ident("x"), Add(Mul(Integer(1), Integer(1)), Div(Integer(1), Integer(2))))))
  }
  it should "not associate == and !=" in {
    parser.parse(
      """begin
        |  bool x = 1 == 1 != 2
        |end
        |""".stripMargin) shouldBe a [Failure[_]]
  }
}
