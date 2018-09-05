package pl.mtpl.tools.ebooks

import java.awt.geom.Rectangle2D
import java.io.StringWriter

import org.apache.pdfbox.pdmodel.{PDDocument, PDPage}
import org.apache.pdfbox.text.PDFTextStripperByArea

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

class PDFProcessor(val reporter: ConvertInfoReporter) {
  val regionStripper: PDFTextStripperByArea = {
    val rs: PDFTextStripperByArea = new PDFTextStripperByArea
    assert(PDFtoEPUB.configuration.isDefined, "Configuration is not defined!")
    val region: Region = PDFtoEPUB.configuration.get.region
    rs.addRegion("content", new Rectangle2D.Double(region.x, region.y, region.weight, region.height))
    rs
  }

  def extractPageContent(document: PDDocument, idx: Int): String = {
    val page: PDPage = document.getPage(idx)
    regionStripper.setStartPage(idx)
    regionStripper.setEndPage(idx)
    regionStripper.extractRegions(page)
    new String(regionStripper.getTextForRegion("content").getBytes("UTF-8"))
  }

  /*
    This nasty piece of logic actually checks, if a given page number is listed
    as a page to be skipped specifically or as a part of array of pages to be skipped
    (for convieniant throwing away all indexes, for example)
   */
  def extractPage(no: Int, skip: Skip): Boolean = {
    !skip.pages.contains(no) && skip.arrays.filter(item => item(0) <= no && item(1) >= no).isEmpty
  }

  /*
    This goes via PDF API and creates pure text.
   */
  def extractTextFromPDF(document: PDDocument): String = {
    val entireBook: StringWriter = new StringWriter
    val skip: Skip = PDFtoEPUB.configuration.get.skip
    for(i <- 0 until document.getNumberOfPages) {
      if(extractPage(i+1, skip)) {
        entireBook.append(extractPageContent(document, i))
      }
    }
    cleanUp(entireBook.toString)
  }

  def cleanUp(entireBook: String): String = {
    var cleanBook: String = entireBook
    val carryOversInOneLine: Regex = """(?i)(?s)([a-z])-\s+([a-z])""".r.unanchored
    var doCleaning: Boolean = true

    println(s"Book to clean contains ${cleanBook.length} characters")

    while(doCleaning) {
      cleanBook = cleanBook match {
        case carryOversInOneLine(left, right) => performCleanup(cleanBook, left, right)
        case _ =>
          doCleaning = false
          cleanBook
      }
    }
    println(s"Book after cleaning contains ${cleanBook.length} characters, replaced $carryOversCnt carry-overs")
    cleanBook
  }

  private var carryOversCnt: Int = 0

  def performCleanup(book: String, left: String, right: String): String = {
    carryOversCnt = carryOversCnt + 1
    book
      .replaceAll(s"$left-\\s+$right", s"$left$right")
  }

  def extractParagraphs(pageContent: String): Seq[String] = {
    val paragraphs: ListBuffer[String] = new ListBuffer[String]
    val lines: Seq[String] = pageContent.split("\n").map(line => line.trim)
    var sw: StringWriter = new StringWriter

    def tag(line: String, tag: String): Unit = {
      paragraphs += sw.toString
      sw = new StringWriter
      sw.append(tag).append("[").append(line).append("] ")
      paragraphs += sw.toString
      sw = new StringWriter
    }

    val chapters: Regex = """(\d+):(.*)""".r
    val parts: Regex = """PART ([A-Z\s]+):(.*)""".r

    lines.foreach {
      case line@chapters(chapter, title) =>
        reporter.addStructure(s"Chapter $chapter : $title")
        tag(line, "CHAPTER")
      case line@parts(part, title) =>
        reporter.addStructure(s"Part $part : $title")
        tag(line, "PART")
      case line =>
        val dotPos: Int = line.lastIndexOf('.')
        sw.append(line).append(" ")
        if (dotPos == line.length - 1) {
          // Very likely next paragraph
          paragraphs += sw.toString
          sw = new StringWriter
        }
    }

    paragraphs += sw.toString
    paragraphs.toList
  }
}
