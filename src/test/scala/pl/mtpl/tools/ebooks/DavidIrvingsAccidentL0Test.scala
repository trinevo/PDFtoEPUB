package pl.mtpl.tools.ebooks

import java.nio.file.{Files, Paths}

import org.scalatest.{Assertions, FlatSpec, Matchers}

class DavidIrvingsAccidentL0Test extends FlatSpec with Matchers {

  "A DavidIrving's book" should "be converted from PDF to EPUB" in {
    PDFtoEPUB.main(Array[String]("src/test/resources/original.pdf", "target/ebook.epub"))
  }

  "A DavidIrving's book" should "be correct" in {
    val template: String = new String(Files.readAllBytes(Paths.get("src/test/resources/ebook.epub.info")))
    val current: String = new String(Files.readAllBytes(Paths.get("target/ebook.epub.info")))
    Assertions.assertResult(template) {
      current
    }
  }

  "Matching chapter names" should "detect chapter" in {
    Assertions.assertResult(List("", "CHAPTER[1: “Soldiers Must Die”] ", "")) {
      new PDFtoEPUBCConverter(null, null).extractParagraphs("1: “Soldiers Must Die”")
    }
  }

  "Matching part names" should "detect part" in {
    Assertions.assertResult(List("", "PART[PART TWO: THE DISASTER] ", "")) {
      new PDFtoEPUBCConverter(null, null).extractParagraphs("PART TWO: THE DISASTER")
    }
  }
}
