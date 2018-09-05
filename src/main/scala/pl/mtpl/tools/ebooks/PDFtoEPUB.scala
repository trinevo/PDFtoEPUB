package pl.mtpl.tools.ebooks

import java.io._
import java.nio.file.{Files, Paths}
import java.util.zip.ZipOutputStream

import net.liftweb.json.DefaultFormats
import org.apache.pdfbox.pdmodel.PDDocument

case class Skip(pages: Array[Int], arrays: Array[Array[Int]])
case class Region(x: Int, y: Int, weight: Int, height: Int)
case class Configuration(pdfFilePath: String, epubFilePath: String, skip: Skip, region: Region)

class PDFtoEPUBCConverter {

  val reporter: ConvertInfoReporter = new ConvertInfoReporter()

  private val epub: EPUBGenerator = new EPUBGenerator(reporter)

  private val pdf: PDFProcessor = new PDFProcessor(reporter)

  def convert(): Unit = {

    val document: PDDocument = PDDocument.load(new File(PDFtoEPUB.configuration.get.pdfFilePath))
    val zip: ZipOutputStream = new ZipOutputStream(new FileOutputStream(PDFtoEPUB.configuration.get.epubFilePath))
    val cleanedBook: String = pdf.extractTextFromPDF(document)

    epub.convert(document, pdf.extractParagraphs(cleanedBook), zip)

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
      if(args(0).endsWith(".json")) {
        implicit val formats = DefaultFormats
        val jsonContent: String = new String(Files.readAllBytes(Paths.get(args(0))))
        Some(net.liftweb.json.parse(jsonContent).extract[Configuration])
      } else {
        Some(new Configuration(args(0), if(args.length > 1) args(1) else s"${args(0)}.epub",
          new Skip(Array.empty[Int], Array.empty[Array[Int]]),
          new Region(0, 0, 0, 0)))
      }
    }

    new PDFtoEPUBCConverter().convert
  }
}
