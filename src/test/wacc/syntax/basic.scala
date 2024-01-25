import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.io.Source

import src.main.wacc.parser
import parsley.{Success, Failure}

class Basic extends AnyFlatSpec {

  private val testDir = "src/test/test_files/syntax/basic/"

  private def parseFile(name: String) =
    parser.parse(Source.fromFile(testDir + name).getLines().mkString("\n"))

  "parser" should "fail with on comment with no #" in {
    parseFile("badComment.wacc") shouldBe a[Failure[_]]
    parseFile("badComment2.wacc") shouldBe a[Failure[_]]
  }

  it should "recognise the valid comment and succeed" in {
    parseFile("goodComment.wacc") shouldBe a[Success[_]]
  }

  it should "not allow bad escape characters" in {
    parseFile("badEscape.wacc") shouldBe a[Failure[_]]
  }

  it should "expect the end keyword for programs" in {
    parseFile("beginNoend.wacc") shouldBe a[Failure[_]]
  }

  it should "fail with syntax error, expect begin" in {
    parseFile("bgnErr.wacc") shouldBe a[Failure[_]]
  }

  it should "fail with syntax error, begin missing closing end" in {
    parseFile("multipleBegins.wacc") shouldBe a[Failure[_]]
  }

  it should "fail with syntax error, missing body" in {
    parseFile("noBody.wacc") shouldBe a[Failure[_]]
  }

  it should "fail with syntax error, skip typo as skp" in {
    parseFile("skpErr.wacc") shouldBe a[Failure[_]]
  }

  it should "fail with syntax error, unescaped double quote" in { // detects wrong error
    parseFile("unescapedChar.wacc") shouldBe a[Success[_]]
  }
}
