package pl.mtpl.tools.ebooks

import org.scalatest.{Assertions, Matchers}

class AccidentBookL0Test extends BookL0Test with Matchers {

  "Accident book" should "be converted from configuration file to EPUB" in {
    PDFtoEPUB.main(Array[String]("src/test/resources/accident/convert.json"))
  }

  "Accident book" should "EPUB generation info" in {
    compareInfos("accident")
  }

  "Matching chapter names" should "detect chapter" in {
    Assertions.assertResult(List("", "CHAPTER[1: “Soldiers Must Die”] ", "page content ")) {
      new PDFProcessor(new ConvertInfoReporter()).extractParagraphs(Seq("1: “Soldiers Must Die”"), "1: “Soldiers Must Die”\npage content")
    }
  }

  "Splitting PDF to files" should "generate only pages for chapters" in {
    verifyHTMLPages("accident")
  }
}
