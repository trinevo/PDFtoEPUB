package pl.mtpl.tools.ebooks

import java.nio.file.{Files, Paths}
import java.util.zip.ZipFile

import difflib.{DiffUtils, Patch}
import org.apache.commons.io.IOUtils
import org.scalatest.{Assertions, FlatSpec, Matchers}

import scala.collection.JavaConverters._

class DavidIrvingsAccidentL0Test extends FlatSpec with Matchers {

  "A DavidIrving's book" should "be converted from PDF direct path to EPUB" in {
    PDFtoEPUB.main(Array[String]("src/test/resources/accident/original.pdf", "target/accident_ebook.epub"))
  }

  "A DavidIrving's book" should "be converted from configuration file to EPUB" in {
    PDFtoEPUB.main(Array[String]("src/test/resources/accident/convert.json"))
  }

  "A DavidIrving's book" should "EPUB generation info" in {
    val template: String = new String(Files.readAllBytes(Paths.get("src/test/resources/accident/ebook.epub.info"))).replaceAll("(?:\\n|\\r)", "")
    val current: String = new String(Files.readAllBytes(Paths.get("target/accident_ebook.epub.info"))).replaceAll("(?:\\n|\\r)", "")

    val templateLines: List[String] = template.split("\n").toList
    val currentLines: List[String] = current.split("\n").toList
    val patch: Patch = DiffUtils.diff(templateLines.asJava, currentLines.asJava)
    val desc: Array[String] = patch.getDeltas.toArray
      .map(delta => delta.toString)

    Assertions.assertResult(Array.empty[String]) {
      desc
    }
  }

  "Matching chapter names" should "detect chapter" in {
    Assertions.assertResult(List("", "CHAPTER[1: “Soldiers Must Die”] ", "")) {
      new PDFProcessor(new ConvertInfoReporter()).extractParagraphs("1: “Soldiers Must Die”")
    }
  }

  "Matching part names" should "detect part" in {
    Assertions.assertResult(List("", "PART[PART TWO: THE DISASTER] ", "")) {
      new PDFProcessor(new ConvertInfoReporter()).extractParagraphs("PART TWO: THE DISASTER")
    }
  }

  "Splitting PDF to files" should "generate only pages for chapters" in {
    val zip: ZipFile = new ZipFile("target/accident_ebook.epub")
    var htmlFiles: Int = 0
    zip.stream().forEach(zipEntry => {
      if(zipEntry.getName.endsWith(".html")) {
        println(s"Entry ${zipEntry.getName}")
        val lines: java.util.List[_] = IOUtils.readLines(zip.getInputStream(zipEntry))
        val titleLine: String = lines.get(10).toString
        Assertions.assert(titleLine.contains("CHAPTER") || titleLine.contains("PART"), s"File name ${zipEntry.getName}, title line ${titleLine}")
      }
    })
  }
}
