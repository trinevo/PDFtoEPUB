package pl.mtpl.tools.ebooks

import org.scalatest.{Assertions, FlatSpec, Matchers}

class DavidIrvingsAccidentL0Test extends FlatSpec with Matchers{

  "A DavidIrving's book" should "be converted from PDF to EPUB" in {
    PDFtoEPUB.main(Array[String]("src/test/resources/original.pdf", "target/ebook.epub"))
  }

  "Matching chapter names" should "detect chapter" in {
    Assertions.assertResult(List("", "CHAPTER[1: “Soldiers Must Die”] ", "")){
      PDFtoEPUB.extractParagraphs("1: “Soldiers Must Die”")
    }
  }

  "Matching part names" should "detect part" in {
    Assertions.assertResult(List("", "PART[PART TWO: THE DISASTER] ", "")){
      PDFtoEPUB.extractParagraphs("PART TWO: THE DISASTER")
    }
  }
}
