package src.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks
import src.main.wacc.{generator, analyser, parser, x86Formatter}

class Generator extends AnyFlatSpec with TableDrivenPropertyChecks {

  behavior of "generator-valid"
  forAll(Table("cases", TestFiles("valid/"): _*)) { file =>
    it should s"not produce errors for ${file.getParentFile}/${file.getName}" in {
      generator.generate((parser.parse(TestFiles.getLines(file)).get), x86Formatter) should not contain ("Exit")
    }
  }

  behavior of "generator-invalid-semanticErr"
  forAll(Table("cases", TestFiles("invalid/semanticErr"): _*)) { file =>
    it should s"produce errors for ${file.getParentFile}/${file.getName}" in {
      generator.generate((parser.parse(TestFiles.getLines(file)).get), x86Formatter) should contain ("Exit: 200")
    }
  }

  behavior of "generator-invalid-syntaxErr"
  forAll(Table("cases", TestFiles("invalid/syntaxErr"): _*)) { file =>
    it should s"produce errors for ${file.getParentFile}/${file.getName}" in {
      generator.generate((parser.parse(TestFiles.getLines(file)).get), x86Formatter) should contain ("Exit: 100")
    }
  }

}
