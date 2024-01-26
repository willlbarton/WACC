import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.io.Source

import src.main.wacc.parser
import parsley.{Success, Failure}

class BasicInvalid extends AnyFlatSpec {

  private val testDir = "src/test/test_files/invalid/syntax/basic/"

  private def parseFile(name: String) =
    parser.parse(Source.fromFile(testDir + name).getLines().mkString)

  "parser" should "fail with on comment with no #" in {
    parseFile("badComment.wacc") shouldBe a [Failure[_]]
    parseFile("badComment2.wacc") shouldBe a [Failure[_]]
  }

  it should "handle bad escaped characters" in {
    parseFile("badEscape.wacc") shouldBe a [Failure[_]]
    parseFile("unescapedChar.wacc") shouldBe a [Failure[_]]
  }

  it should "expect the parse begin and end keyword for programs" in {
    parseFile("beginNoend.wacc") shouldBe a [Failure[_]]
    parseFile("bgnErr.wacc") shouldBe a [Failure[_]]
  }

  it should "not allow multiple programs" in {
    parseFile("multipleBegins.wacc") shouldBe a [Failure[_]]
  }

  it should "expect a body in programs" in {
    parseFile("noBody.wacc") shouldBe a [Failure[_]]
  }

  it should "parse skip keyword" in {
    parseFile("skpErr.wacc") shouldBe a [Failure[_]]
  }

}
