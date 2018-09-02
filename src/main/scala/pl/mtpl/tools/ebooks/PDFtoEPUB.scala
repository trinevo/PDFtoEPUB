package pl.mtpl.tools.ebooks

import java.awt.geom.Rectangle2D
import java.io._
import java.util.zip.{ZipEntry, ZipOutputStream}

import org.apache.pdfbox.pdmodel.{PDDocument, PDPage}
import org.apache.pdfbox.text.{PDFTextStripper, PDFTextStripperByArea}

object PDFtoEPUB {

  private def beginFile(bw: StringWriter): Unit = {
    bw.write("""<?xml version='1.0' encoding='utf-8'?>
               |<html xmlns="http://www.w3.org/1999/xhtml" lang="" xml:lang="">
               |  <head>
               |    <title>Accident: The Death of General Sikorski</title>
               |    <meta name="generator" content="PDFtoEPUB 1.0"/>
               |    <meta name="author" content="David Irving"/>
               |    <meta name="date" content="2007-06-11T11:05:49+00:00"/>
               |    <meta name="subject" content="Accident"/>
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

  private def prepareFiles(document: PDDocument) : collection.Seq[String] = {

    var sw: StringWriter = new StringWriter

    val textStripper: PDFTextStripper = new PDFTextStripper
    val regionStripper: PDFTextStripperByArea = new PDFTextStripperByArea
    val contentRegion: Rectangle2D = new Rectangle2D.Double(0, 80, 550, 520)
    regionStripper.addRegion("content", contentRegion)

    val pageCnt: Int = document.getNumberOfPages
    val pagesInFile: Int = 10

    val files: collection.mutable.ListBuffer[String] = new collection.mutable.ListBuffer[String]
    beginFile(sw)
    for(i <- 0 until pageCnt) {
      val page: PDPage = document.getPage(i)
      regionStripper.setStartPage(i)
      regionStripper.setEndPage(i)
      regionStripper.extractRegions(page)
      val pageContent: String = regionStripper.getTextForRegion("content")


      sw.write("<p>")
      sw.write(pageContent)
      sw.write("</p>\n")

      if(i > 0 && i % pagesInFile == 0) {
        endFile(sw)
        files += sw.toString
        sw = new StringWriter
        beginFile(sw)
      }
    }
    endFile(sw)
    files += sw.toString
    files
  }

  def main(args: Array[String]): Unit = {
    println(s"Run converter for PDF file ${args(0)}")
    val document: PDDocument = PDDocument.load(new File(args(0)))
    println(s"Read document with pages: ${document.getNumberOfPages}")

    val zip: ZipOutputStream = new ZipOutputStream(new FileOutputStream("ebook.epub"))
    val manifest: StringBuffer = new StringBuffer
    manifest.append("  <manifest>\n")

    val toc: StringBuffer = new StringBuffer
    toc.append("  <spine toc=\"ncx\">\n")

    val files: Seq[String] = prepareFiles(document)
    var idx: Int = 0
    println(s"Prepared files ${files.size}")
    files.foreach(file => {
      idx = idx + 1
      addZipEntry(zip, s"content${idx}.html", file)
      manifest.append(s"    <item href=")
        .append('"').append("content").append(idx).append(".html").append('"')
        .append(" id=").append('"').append("id").append(idx).append('"')
        .append(" media-type=").append('"').append("application/xhtml+xml").append('"').append("/>").append('\n')

      toc.append(s"    <itemref idref=").append('"').append("id").append(idx).append('"').append("/>").append('\n')
    })

    manifest.append("  </manifest>")
    toc.append("  </spine>")

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
                                      |    <dc:date>${document.getDocumentInformation.getModificationDate}</dc:date>
                                      |    <dc:identifier id="uuid_id" opf:scheme="uuid">5de5766d-0eb5-4041-9518-54ceedf99614</dc:identifier>
                                      |    <dc:contributor opf:role="bkp">PDFtoEPUB</dc:contributor>
                                      |  </metadata>
                                      |${manifest}
                                      |${toc}
                                      |  <guide/>
                                      |</package>
                                      |""".stripMargin)
    zip.close
  }

  def addZipEntry(zip: ZipOutputStream, fileName: String, content: String): Unit = {
    val entry: ZipEntry = new ZipEntry(fileName)
    zip.putNextEntry(entry)
    zip.write(content.getBytes)
  }
}
