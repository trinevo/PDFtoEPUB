package pl.mtpl.tools.ebooks

import java.io._
import java.nio.file.{Files, Paths}
import java.util.zip.ZipOutputStream

import net.liftweb.json.DefaultFormats
import org.apache.pdfbox.pdmodel.PDDocument

case class Skip(pages: Array[Int], arrays: Array[Array[Int]])
case class Region(x: Int, y: Int, weight: Int, height: Int)
case class ContentsIndex(pages: Array[Int], chaptersRegExp: String)
case class Debug(enabled: Boolean, paragraphs: Array[Int], lines: Array[Int])
case class Configuration(pdfFilePath: String,
                         epubFilePath: String,
                         encoding: String,
                         skip: Skip,
                         region: Region,
                         contentsIndex: ContentsIndex,
                         debug: Debug)

class PDFtoEPUBCConverter {

  def convert(): Unit = {

    val document: PDDocument = PDDocument.load(new File(PDFtoEPUB.configuration.get.pdfFilePath))
    val zip: ZipOutputStream = new ZipOutputStream(new FileOutputStream(PDFtoEPUB.configuration.get.epubFilePath))

    val reporter: ConvertInfoReporter = new ConvertInfoReporter()

    new EPUBGenerator(reporter).convert(document, zip)

    document.close()
    zip.close()

    reporter.generate()
  }
}

object PDFtoEPUB {
  var configuration: Option[Configuration] = None

  def main(args: Array[String]): Unit = {
    println(s"Run converter for file ${args(0)}")

    configuration = {
      implicit val formats = DefaultFormats
      val jsonContent: String = new String(Files.readAllBytes(Paths.get(args(0))))
      Some(net.liftweb.json.parse(jsonContent).extract[Configuration])
    }

    new PDFtoEPUBCConverter().convert
  }

  def debugParagraph(paragraphNo: Int): Boolean = {
    configuration.get.debug.enabled &&
      configuration.get.debug.paragraphs.size > 1 &&
      paragraphNo >= configuration.get.debug.paragraphs(0) &&
      paragraphNo <= configuration.get.debug.paragraphs(1)
  }

  def debugLine(lineNo: Int): Boolean = {
    configuration.get.debug.enabled &&
      configuration.get.debug.lines.size > 1 &&
      lineNo >= configuration.get.debug.lines(0) &&
      lineNo <= configuration.get.debug.lines(1)
  }
}
