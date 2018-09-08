package pl.mtpl.tools.ebooks

import java.awt.geom.Rectangle2D
import java.io.StringWriter
import java.nio.charset.Charset

import org.apache.pdfbox.pdmodel.{PDDocument, PDPage}
import org.apache.pdfbox.text.PDFTextStripperByArea

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.matching.Regex

class PDFProcessor(val reporter: ConvertInfoReporter) {
  val regionStripper: PDFTextStripperByArea = {
    val rs: PDFTextStripperByArea = new PDFTextStripperByArea
    assert(PDFtoEPUB.configuration.isDefined, "Configuration is not defined!")
    val region: Region = PDFtoEPUB.configuration.get.region
    rs.addRegion("content", new Rectangle2D.Double(region.x, region.y, region.weight, region.height))
    rs
  }

  def extractPageContent(document: PDDocument, idx: Int, enc: String): String = {
    val page: PDPage = document.getPage(idx)
    regionStripper.setStartPage(idx)
    regionStripper.setEndPage(idx)
    regionStripper.extractRegions(page)
    new String(regionStripper.getTextForRegion("content").getBytes(Charset.forName(enc)))
  }

  /*
    This nasty piece of logic actually checks, if a given page number is listed
    as a page to be skipped specifically or as a part of array of pages to be skipped
    (for convieniant throwing away all indexes, for example)
   */
  def extractPage(no: Int, skip: Skip): Boolean = {
    !skip.pages.contains(no) && skip.arrays.filter(item => item(0) <= no && item(1) >= no).isEmpty
  }

  def extractChapterListFromPDF(document: PDDocument): Seq[String] = {
    val chapterPage: StringWriter = new StringWriter
    val index: ContentsIndex = PDFtoEPUB.configuration.get.contentsIndex
    val enc: String = PDFtoEPUB.configuration.get.encoding
    Charset.forName(enc)
    index.pages.foreach(pageNo => {
      val page: String = extractPageContent(document, pageNo-1, enc)
      chapterPage.append(page).append('\n')
      reporter.addBatch(s"Chapter list page $pageNo", page)
    })

    val ab: ArrayBuffer[String] = new ArrayBuffer[String]()
    chapterPage.toString.split("\n").foreach(line => {
      index.chaptersRegExp.r.findAllIn(line).matchData.foreach(matched => {
        ab.append(matched.group("chapter"))
      })
    })
    ab
  }

  /*
    This goes via PDF API and creates pure text.
   */
  def extractTextFromPDF(document: PDDocument): String = {
    val entireBook: StringWriter = new StringWriter
    val skip: Skip = PDFtoEPUB.configuration.get.skip
    val enc: String = PDFtoEPUB.configuration.get.encoding
    for(i <- 0 until document.getNumberOfPages) {
      if(extractPage(i+1, skip)) {
        entireBook.append(extractPageContent(document, i, enc))
      }
    }
    cleanUp(entireBook.toString)
  }

  def cleanUp(entireBook: String): String = {
    var cleanBook: String = entireBook
    val carryOversInOneLine: Regex = """(?i)(?s)([a-z])-\s+([a-z])""".r.unanchored
    var doCleaning: Boolean = true

    reporter.addStructure(s"Characters in book : ${cleanBook.length}")

    while(doCleaning) {
      cleanBook = cleanBook match {
        case carryOversInOneLine(left, right) => performCleanup(cleanBook, left, right)
        case _ =>
          doCleaning = false
          cleanBook
      }
    }
    reporter.addStructure(s"Characters in leaned book : ${cleanBook.length}")
    reporter.addStructure(s"Replaced carry-overs : $carryOversCnt")
    cleanBook
  }

  private var carryOversCnt: Int = 0

  def performCleanup(book: String, left: String, right: String): String = {
    carryOversCnt = carryOversCnt + 1
    book
      .replaceAll(s"$left-\\s+$right", s"$left$right")
  }

  private var lineNo: Int = 0

  def extractParagraphs(chaptersTitles: Seq[String], pageContent: String): Seq[String] = {
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

    if(PDFtoEPUB.configuration.get.debug.enabled) {
      chaptersTitles.foreach(chapter => println(s"DEBUG PAGE |> Chapter title $chapter"))
    }
    lines.foreach(line => {
      lineNo = lineNo + 1
      if(PDFtoEPUB.debugLine(lineNo)) {
        println(s"DEBUG PAGE |> Line >>$line<<")
      }
      if(chaptersTitles.contains(line.trim)) {
        reporter.addStructure(s"Chapter $line")
        tag(line, "CHAPTER")
        if(PDFtoEPUB.debugLine(lineNo)) {
          println(s"DEBUG PAGE |> Mark as chapter")
        }
      } else {
        val dotPos: Int = line.lastIndexOf('.')
        sw.append(line).append(" ")
        if (dotPos == line.length - 1) {
          // Very likely next paragraph
          paragraphs += sw.toString
          sw = new StringWriter
        }
      }
    })

    paragraphs += sw.toString
    paragraphs.toList
  }
}
