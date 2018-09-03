package pl.mtpl.tools.ebooks

import java.io.{BufferedWriter, FileWriter, StringWriter}

import scala.util.hashing.MurmurHash3

class ConvertInfoReporter(val epubFilePath: String) {

  private val generables: collection.mutable.Map[String, Int] = collection.mutable.TreeMap[String, Int]()

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

  def reportEntry(fileName: String, content: String): Unit = {
    generables.put(fileName, MurmurHash3.bytesHash(content.getBytes))
  }

  def generate(): Unit = {
    val fow: FileWriter = new FileWriter(s"${epubFilePath}.info")
    val bw: BufferedWriter = new BufferedWriter(fow)

    bw.append(preambule.toString)
    bw.append(structure.toString)

    bw.append(s"Contains ${generables.size} pages\n")

    generables.foreach(entry => {
      val sw: StringWriter = new StringWriter
      sw.write(entry._1)
      sw.write(" => ")
      sw.write(entry._2.toString)
      sw.write('\n')
      bw.write(sw.toString)
    })

    bw.close()
    fow.close()
  }
}
