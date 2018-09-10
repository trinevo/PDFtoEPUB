package pl.mtpl.tools.ebooks

import java.io.StringWriter
import java.util.zip.{ZipEntry, ZipOutputStream}

import org.apache.pdfbox.pdmodel.PDDocument

class EPUBGenerator(reporter: ConvertInfoReporter) {

  private val pdf: PDFProcessor = new PDFProcessor(reporter)

  private val manifest: StringBuffer = new StringBuffer

  private val spine: StringBuffer = new StringBuffer

  private val toc: StringBuffer = new StringBuffer

  def generateDescriptors(zip: ZipOutputStream, document: PDDocument): Unit = {
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
    reporter.addPreambule(s"Pages  : ${document.getNumberOfPages}")

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

  private def addZipEntry(zip: ZipOutputStream, fileName: String, content: String): Unit = {
    val entry: ZipEntry = new ZipEntry(fileName)
    zip.putNextEntry(entry)
    zip.write(content.getBytes)

    reporter.reportEntry(fileName, content)
  }

  def generateContentHTMLFiles(zip: ZipOutputStream): Unit = {
    var idx: Int = 0
    println(s"Prepared files ${files.size}")

    manifest.append("  <manifest>\n")
    spine.append("  <spine toc=\"ncx\">\n")
    toc.append("<navMap>\n")

    files.foreach(file => {
      idx = idx + 1
      val fileName: String = s"content$idx.html"
      val id: String = s"id$idx"
      addZipEntry(zip, s"content$idx.html", file._2)
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
        .append("        <text>").append(file._1).append("</text>\n")
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
      """  </body>
        |</html>""".stripMargin)
  }

  val files: collection.mutable.Map[String, String] = new collection.mutable.LinkedHashMap[String, String]

  private var thereIsAContent: Boolean = false

  private def endFileProcessing(fileContent: StringWriter, name: String): Unit = {
    if(thereIsAContent) {
      endFile(fileContent)
      var idx: Int = 0
      var search: Boolean = true
      while(search) {
        val mapName = s"${name}.$idx"
        if(!files.contains(mapName)) {
          files.put(mapName, fileContent.toString)
          search = false
        } else {
          idx = idx + 1
        }
      }
    }
    thereIsAContent = false
  }

  private def writeHTMLParagraph(fileContent: StringWriter, paragraph: String): Unit = {
    fileContent.write("<p class=")
    fileContent.write('"')
    fileContent.write("calibre1")
    fileContent.write('"')
    fileContent.write(">")
    fileContent.write(paragraph)
    fileContent.write("</p>\n")
  }

  var paragraphNo: Int = 0

  def prepareFiles(document: PDDocument, paragraphs: Seq[String]) : Unit = {
    var name: String = "CHAPTER[ARTIFICIAL PROLOG]"
    var fileContent: StringWriter = new StringWriter
    beginFile(document, fileContent)
    writeHTMLParagraph(fileContent, name)

    paragraphs.foreach(paragraph => {
      paragraphNo = paragraphNo + 1
      if(PDFtoEPUB.debugParagraph(paragraphNo)) {
        println(s"DEBUG Paragraph ${paragraphNo}|>")
        println(paragraph)
      }
      if(paragraph.contains("CHAPTER[")) {
        if(PDFtoEPUB.debugParagraph(paragraphNo)) {
          println(s"DEBUG Paragraph ${paragraphNo}|> It is a CHAPTER header, ending previous paragraph $name")
        }
        endFileProcessing(fileContent, name)
        name = paragraph
        fileContent = new StringWriter
        beginFile(document, fileContent)
      } else if(fileContent.toString.size > PDFtoEPUB.configuration.get.htmlLimit) {
        endFileProcessing(fileContent, name)
        fileContent = new StringWriter
        beginFile(document, fileContent)
      }

      if(!paragraph.isEmpty) {
        thereIsAContent = true
        writeHTMLParagraph(fileContent, paragraph)
      }
    })

    endFileProcessing(fileContent, name)
  }

  def convert(document: PDDocument, zip: ZipOutputStream): Unit = {
    val chapterListPages: Seq[String] = pdf.extractChapterListFromPDF(document)
    val cleanedBook: String = pdf.extractTextFromPDF(document)
    val paragraphs: Seq[String] = pdf.extractParagraphs(chapterListPages, cleanedBook)
    prepareFiles(document, paragraphs)
    generateContentHTMLFiles(zip)
    generateDescriptors(zip, document)
  }
}
