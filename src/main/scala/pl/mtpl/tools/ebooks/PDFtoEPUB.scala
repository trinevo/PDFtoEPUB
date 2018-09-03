package pl.mtpl.tools.ebooks

import java.awt.geom.Rectangle2D
import java.io._
import java.util.zip.{ZipEntry, ZipOutputStream}

import org.apache.pdfbox.pdmodel.{PDDocument, PDPage}
import org.apache.pdfbox.text.PDFTextStripperByArea

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

class PDFtoEPUBCConverter(val pdfFilePath: String, val epubFilePath: String) {

  val reporter: ConvertInfoReporter = new ConvertInfoReporter(epubFilePath)

  val regionStripper: PDFTextStripperByArea = {
    val rs: PDFTextStripperByArea = new PDFTextStripperByArea
    rs.addRegion("content", new Rectangle2D.Double(0, 80, 550, 520))
    rs
  }

  private def beginFile(document: PDDocument, bw: StringWriter): Unit = {
    bw.write(s"""<?xml version='1.0' encoding='utf-8'?>
               |<html xmlns="http://www.w3.org/1999/xhtml" lang="" xml:lang="">
               |  <head>
               |    <title>${document.getDocumentInformation.getTitle}</title>
               |    <meta name="generator" content="PDFtoEPUB 1.0"/>
               |    <meta name="author" content="${document.getDocumentInformation.getAuthor}"/>
               |    <meta name="subject" content="${document.getDocumentInformation.getSubject}"/>
               |    <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
               |  </head>
               |  <body>
               |""".stripMargin)
  }

  private def endFile(bw: StringWriter): Unit = {
    bw.write(
      """
        |  </body>
        |</html>
        |""".stripMargin)
  }

  private def extractPageContent(document: PDDocument, idx: Int): String = {
    val page: PDPage = document.getPage(idx)
    regionStripper.setStartPage(idx)
    regionStripper.setEndPage(idx)
    regionStripper.extractRegions(page)
    regionStripper.getTextForRegion("content")
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

    val chapters: Regex = """(\d+):.*""".r
    val parts: Regex = """PART ([A-Z\s]+):.*""".r

    lines.foreach {
      case line@chapters(chapter) =>
        reporter.addStructure(s"Chapter $chapter")
        tag(line, "CHAPTER")
      case line@parts(part) =>
        reporter.addStructure(s"Part $part")
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

  private var carryOversCnt: Int = 0

  def performCleanup(book: String, left: String, right: String): String = {
    carryOversCnt = carryOversCnt + 1
    book
      .replaceAll(s"$left-\\s+$right", s"$left$right")
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

  /*
    This goes via PDF API and creates pure text.
   */
  private def extractTextFromPDF(document: PDDocument): String = {
    val entireBook: StringWriter = new StringWriter
    for(i <- 0 until document.getNumberOfPages) {
      entireBook.append(extractPageContent(document, i))
    }
    entireBook.toString
  }

  private def prepareFiles(document: PDDocument) : collection.Seq[String] = {
    val files: collection.mutable.ListBuffer[String] = new collection.mutable.ListBuffer[String]
    val cleanedBook: String = cleanUp(extractTextFromPDF(document))

    var fileContent: StringWriter = new StringWriter
    beginFile(document, fileContent)

    var idx = 0

    extractParagraphs(cleanedBook.toString).foreach(paragraph => {
      idx = idx + 1

      fileContent.write("<p class=")
      fileContent.write('"')
      fileContent.write("calibre1")
      fileContent.write('"')
      fileContent.write(">")
      fileContent.write(paragraph)
      fileContent.write("</p>\n")

      if(idx % 50 == 0) {
        endFile(fileContent)
        files += fileContent.toString
        fileContent = new StringWriter
        beginFile(document, fileContent)
      }
    })
    endFile(fileContent)
    files += fileContent.toString
    files
  }

  def generateContentHTMLFiles(zip: ZipOutputStream, files: Seq[String], manifest: StringBuffer, spine: StringBuffer, toc: StringBuffer): Unit = {
    var idx: Int = 0
    println(s"Prepared files ${files.size}")

    manifest.append("  <manifest>\n")
    spine.append("  <spine toc=\"ncx\">\n")
    toc.append("<navMap>\n")

    files.foreach(file => {
      idx = idx + 1
      val fileName: String = s"content$idx.html"
      val id: String = s"id$idx"
      addZipEntry(zip, s"content$idx.html", file)
      manifest.append(s"    <item href=")
        .append('"').append(fileName).append('"')
        .append(" id=").append('"').append(id).append('"')
        .append(" media-type=").append('"').append("application/xhtml+xml").append('"')
        .append("/>").append('\n')

      spine.append(s"    <itemref idref=")
        .append('"').append(id).append('"')
        .append("/>").append('\n')

      toc.append(s"    <navPoint id=").append('"').append(id).append('"').append(" playOrder=").append('"').append(idx).append('"').append(">").append('\n')
        .append("      <navLabel>\n")
        .append("        <text>Part ").append(idx).append("</text>\n")
        .append("      </navLabel>\n")
        .append("      <content src=").append('"').append(fileName).append('"').append("/>\n")
        .append("    </navPoint>\n")
    })

    manifest.append("    <item href=").append('"').append("page_styles.css").append('"')
    .append(" id=").append('"').append("page_css").append('"').append(" media-type=").append('"')
    .append("text/css").append('"').append("/>\n")

    manifest.append("    <item href=").append('"').append("stylesheet.css").append('"')
      .append(" id=").append('"').append("page_css").append('"').append(" media-type=").append('"')
      .append("text/css").append('"').append("/>\n")

    manifest.append("    <item href=").append('"').append("toc.ncx").append('"')
      .append(" id=").append('"').append("ncx").append('"').append(" media-type=").append('"')
      .append("application/x-dtbncx+xml").append('"').append("/>\n")

    manifest.append("  </manifest>")
    spine.append("  </spine>")
    toc.append("  </navMap>")
  }

  def generateDescriptors(zip: ZipOutputStream, document: PDDocument, manifest: StringBuffer, spine: StringBuffer, toc: StringBuffer): Unit = {
    addZipEntry(zip, "META-INF/container.xml", """<?xml version="1.0"?>
                                                 |<container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
                                                 |   <rootfiles>
                                                 |      <rootfile full-path="content.opf" media-type="application/oebps-package+xml"/>
                                                 |   </rootfiles>
                                                 |</container>""".stripMargin)

    addZipEntry(zip, "content.opf", s"""<?xml version='1.0' encoding='utf-8'?>
                                       |<package xmlns="http://www.idpf.org/2007/opf" unique-identifier="uuid_id" version="2.0">
                                       |  <metadata xmlns:calibre="http://calibre.kovidgoyal.net/2009/metadata" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:opf="http://www.idpf.org/2007/opf" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                                       |    <dc:title>${document.getDocumentInformation.getTitle}</dc:title>
                                       |    <dc:description>${document.getDocumentInformation.getKeywords}</dc:description>
                                       |    <dc:creator opf:file-as="Unknown" opf:role="aut">${document.getDocumentInformation.getAuthor}</dc:creator>
                                       |    <dc:subject>${document.getDocumentInformation.getSubject}</dc:subject>
                                       |    <dc:date>None</dc:date>
                                       |    <dc:identifier id="uuid_id" opf:scheme="uuid">5de5766d-0eb5-4041-9518-54ceedf99614</dc:identifier>
                                       |    <dc:contributor opf:role="bkp">PDFtoEPUB</dc:contributor>
                                       |  </metadata>
                                       |$manifest
                                       |$spine
                                       |  <guide/>
                                       |</package>
                                       |""".stripMargin)

    reporter.addPreambule(s"Title  : ${document.getDocumentInformation.getTitle}")
    reporter.addPreambule(s"Author : ${document.getDocumentInformation.getAuthor}")
    reporter.addPreambule(s"Subject: ${document.getDocumentInformation.getSubject}")

    addZipEntry(zip, "toc.ncx", s"""<?xml version='1.0' encoding='utf-8'?>
                                  |<ncx xmlns="http://www.daisy.org/z3986/2005/ncx/" version="2005-1" xml:lang="eng">
                                  |  <head>
                                  |    <meta content="5de5766d-0eb5-4041-9518-54ceedf99614" name="dtb:uid"/>
                                  |    <meta content="2" name="dtb:depth"/>
                                  |    <meta content="calibre (3.27.1)" name="dtb:generator"/>
                                  |    <meta content="${document.getNumberOfPages}" name="dtb:totalPageCount"/>
                                  |    <meta content="${document.getNumberOfPages}" name="dtb:maxPageNumber"/>
                                  |  </head>
                                  |  <docTitle>
                                  |    <text>${document.getDocumentInformation.getTitle}</text>
                                  |  </docTitle>
                                  |  $toc
                                  |</ncx>""".stripMargin)

    addZipEntry(zip, "mimetype", "application/epub+zip")
  }

  def addZipEntry(zip: ZipOutputStream, fileName: String, content: String): Unit = {
    val entry: ZipEntry = new ZipEntry(fileName)
    zip.putNextEntry(entry)
    zip.write(content.getBytes)

    reporter.reportEntry(fileName, content)
  }

  def convert(): Unit = {

    val document: PDDocument = PDDocument.load(new File(pdfFilePath))
    println(s"Read document with pages: ${document.getNumberOfPages}")

    val zip: ZipOutputStream = new ZipOutputStream(new FileOutputStream(epubFilePath))

    val manifest: StringBuffer = new StringBuffer
    val spine: StringBuffer = new StringBuffer
    val toc: StringBuffer = new StringBuffer

    generateContentHTMLFiles(zip, prepareFiles(document), manifest, spine, toc)

    generateDescriptors(zip, document, manifest, spine, toc)

    document.close()
    zip.close()

    reporter.generate()
  }
}

object PDFtoEPUB {
  def main(args: Array[String]): Unit = {
    println(s"Run converter for PDF file ${args(0)}")
    println(s"Result file ${args(1)}")

    new PDFtoEPUBCConverter(args(0), args(1)).convert()
  }
}
