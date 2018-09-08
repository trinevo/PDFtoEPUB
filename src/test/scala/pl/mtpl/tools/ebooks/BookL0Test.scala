package pl.mtpl.tools.ebooks

import java.nio.file.{Files, Paths}
import java.util.zip.ZipFile

import difflib.{DiffUtils, Patch}
import org.apache.commons.io.IOUtils
import org.scalatest.{Assertions, FlatSpec}

import scala.collection.JavaConverters._

class BookL0Test extends FlatSpec {

  def compareInfos(name: String): Unit = {
    val template: String = new String(Files.readAllBytes(Paths.get(s"src/test/resources/$name/ebook.epub.info"))).replaceAll("(?:\\n|\\r)", "")
    val current: String = new String(Files.readAllBytes(Paths.get(s"target/${name}_ebook.epub.info"))).replaceAll("(?:\\n|\\r)", "")

    val templateLines: List[String] = template.split("\n").toList
    val currentLines: List[String] = current.split("\n").toList
    val patch: Patch = DiffUtils.diff(templateLines.asJava, currentLines.asJava)
    val desc: Array[String] = patch.getDeltas.toArray
    .map(delta => delta.toString)

    Assertions.assertResult(Array.empty[String]) {
      desc
    }
  }

  def verifyHTMLPages(name: String): Unit = {
    val zip: ZipFile = new ZipFile(s"target/${name}_ebook.epub")
    zip.stream().forEach(zipEntry => {
      if(zipEntry.getName.endsWith(".html")) {
        val lines: java.util.List[_] = IOUtils.readLines(zip.getInputStream(zipEntry))
        val titleLine: String = lines.get(10).toString
        Assertions.assert(titleLine.contains("CHAPTER") || titleLine.contains("PART"), s"File name ${zipEntry.getName}, title line ${titleLine}")
      }
    })
  }
}
