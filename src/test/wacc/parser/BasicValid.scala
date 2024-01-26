import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.io.Source

import src.main.wacc._
import parsley.{Success, Failure}

class BasicValid extends AnyFlatSpec {

  private val testDir = "src/test/test_files/valid/basic/"

  private def parseFile(name: String) =
    parser.parse(Source.fromFile(testDir + name).getLines().mkString)

  "parser" should "be able to parse a basic program" in {
    parseFile("skip/justSkip.wacc") shouldBe Success(Program(Nil, Skip))
  }

  it should "be able to parse a basic program with comments" in {
    parseFile("skip/comment.wacc") shouldBe Success(Program(Nil, Skip))
    parseFile("skip/skip.wacc") shouldBe Success(Program(Nil, Skip))
  }

  it should "allow comments at the end of a file" in {
    parseFile("skip/commentEoF.wacc") shouldBe Success(Program(Nil, Skip))
  }

  it should "allow comments inline" in {
    parseFile("skip/commentInLine.wacc") shouldBe Success(Program(Nil, Skip))
  }

  it should "should handle exit statements" in {
    parseFile("exit/exitBasic.wacc") shouldBe Success(Program(Nil, Exit(Integer(7))))
    parseFile("exit/exitBasic2.wacc") shouldBe Success(Program(Nil, Exit(Integer(42))))
    parseFile("exit/exitWrap.wacc") shouldBe Success(Program(Nil, Exit(Integer(256))))
  }

  it should "handle exit statements with negative numbers" in {
    parseFile("exit/exit-1.wacc") shouldBe Success(Program(Nil, Exit(UnaryApp(Neg, Integer(1)))))
  }

}
