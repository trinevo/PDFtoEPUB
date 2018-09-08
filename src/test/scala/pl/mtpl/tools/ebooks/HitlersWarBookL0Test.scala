package pl.mtpl.tools.ebooks

import org.scalatest.{Assertions, Matchers}

class HitlersWarBookL0Test extends BookL0Test with Matchers {

  "Hitler's War book" should "be converted from configuration file to EPUB" in {
    PDFtoEPUB.main(Array[String]("src/test/resources/hitlerswar/convert.json"))
  }

  "Hitler's War book" should "EPUB generation info" in {
    compareInfos("hitlerswar")
  }

  "Matching chapter names" should "detect chapter" in {
    Assertions.assertResult(List("CHAPTER[Introduction] ")) {
      new PDFProcessor(new ConvertInfoReporter()).extractParagraphs(Seq("Introduction"), "CHAPTER[Introduction]")
    }
  }

  "Matching part names" should "detect part" in {
    Assertions.assertResult(List("PART[CZESC PIERWSZA] ")) {
      new PDFProcessor(new ConvertInfoReporter()).extractParagraphs(Seq("CZESC PIERWSZA"), "PART[CZESC PIERWSZA]")
    }
  }

  "Splitting PDF to files" should "generate only pages for chapters" in {
    verifyHTMLPages("hitlerswar")
  }
}
