package pl.mtpl.tools.ebooks

import java.io.{BufferedWriter, FileWriter, StringWriter}
import java.util

import scala.util.hashing.MurmurHash3

class ConvertInfoReporter {

  private val generables: collection.mutable.Map[String, (Int, Int)] = collection.mutable.TreeMap[String, (Int, Int)]()

  private val preambule: StringWriter = new StringWriter

  def addPreambule(entry: String): Unit = {
    preambule.write("Doc info: ")
    preambule.write(entry)
    preambule.write('\n')
  }

  val structure: StringWriter = new StringWriter

  def addStructure(entry: String): Unit = {
    structure.write("Structure: ")
    structure.write(entry)
    structure.write('\n')
  }

  val batches: StringWriter = new StringWriter

  def addBatch(batchType: String, entry: String): Unit = {
    batches.write("--- ")
    batches.write(batchType)
    batches.write('\n')
    batches.write("Size: ")
    batches.write(entry.size.toString)
    batches.write('\n')
    batches.write(entry)
    batches.write("---\n")
  }

  def reportEntry(fileName: String, content: String): Unit = {
    generables.put(fileName, (content.size, MurmurHash3.bytesHash(content.getBytes)))
  }

  def generate(): Unit = {
    val fow: FileWriter = new FileWriter(s"${PDFtoEPUB.configuration.get.epubFilePath}.info")
    val bw: BufferedWriter = new BufferedWriter(fow)

    implicit def confToString(x: Configuration): String = {
      new StringWriter()
        .append("pdfFilePath=").append(x.pdfFilePath).append("\n")
        .append("epubFilePath=").append(x.epubFilePath).append("\n")
        .append("skip={").append(x.skip).append("}\n")
        .append("region={").append(x.region).append("}\n")
        .append("contentsIndex={").append(x.contentsIndex).append("}\n")
        .toString
    }

    implicit def skipToString(x: Skip): String = {
      new StringWriter()
        .append("pages=[")
        .append(util.Arrays.toString(x.pages))
        .append("], arrays=[")
        .append(if(x.arrays.isEmpty) "" else x.arrays.map(arr => util.Arrays.toString(arr)).reduce((sum, item) => s"${sum}, ${item}"))
        .append("]")
        .toString
    }

    implicit def regionToString(r: Region): String = {
      new StringWriter()
        .append("dimensions=[")
        .append("x=")
        .append(r.x.toString)
        .append(", y=")
        .append(r.y.toString)
        .append(", weigth=")
        .append(r.weight.toString)
        .append(", heigth=")
        .append(r.height.toString)
        .append("]")
        .toString
    }

    implicit def contentsIndexToString(r: ContentsIndex): String = {
      new StringWriter()
        .append("pages=[")
        .append(util.Arrays.toString(r.pages))
        .append("], chaptersRegExp=")
        .append(r.chaptersRegExp)
        .append("]")
        .toString
    }

    bw.append(PDFtoEPUB.configuration.get)
    bw.append(preambule.toString)
    bw.append(structure.toString)
    bw.append(batches.toString)

    bw.append(s"Contains ${generables.size} zip entries (files)\n")

    var counter: Int = 0
    generables.foreach(entry => {
      counter = counter + 1
      val sw: StringWriter = new StringWriter
      sw.write(counter.toString)
      sw.write(". ")
      sw.write(entry._1)
      sw.write(" => ")
      sw.write(entry._2._1.toString)
      sw.write(" lines, hash=")
      sw.write(entry._2._2.toString)
      sw.write('\n')
      bw.write(sw.toString)
    })

    bw.close()
    fow.close()
  }
}
