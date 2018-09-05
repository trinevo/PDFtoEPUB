package pl.mtpl.tools.ebooks

import org.scalatest.{Assertions, FlatSpec, Matchers}

class PDFProcessorL0Test extends FlatSpec with Matchers {
  PDFtoEPUB.configuration = Some(new Configuration("", "",
    new Skip(Array.empty[Int], Array.empty[Array[Int]]),
    new Region(0, 80, 550, 520)))

  "Clean up book" should "remove all one line carry-overs" in {
    Assertions.assertResult("This is reassigned text that contains no carry-over words left after PDF formatted pages."){
      new PDFProcessor(new ConvertInfoReporter()).cleanUp("This is reassi- gned text that cont- ains no carry-over words left after PDF for- matted pages.")
    }
  }

  "Clean up book" should "remove all two lines carry-overs" in {
    Assertions.assertResult("This is reassigned text that contains no carry-over words left after PDF formatted pages."){
      new PDFProcessor(new ConvertInfoReporter()).cleanUp(
        """This is reassi-
          |gned text that cont-
          | ains no carry-over words left after PDF for-
          | matted pages.""".stripMargin)
    }
  }

  "Extract text" should "avoiding pages 1, 2, and between 10 and 30" in {
    val skip: Skip = new Skip(Array(1, 2), Array[Array[Int]](Array[Int](10, 30)))
    Assertions.assertResult(false){
      new PDFProcessor(new ConvertInfoReporter()).extractPage(1, skip)
    }
    Assertions.assertResult(false){
      new PDFProcessor(new ConvertInfoReporter()).extractPage(2, skip)
    }
    Assertions.assertResult(false){
      new PDFProcessor(new ConvertInfoReporter()).extractPage(20, skip)
    }
    Assertions.assertResult(true){
      new PDFProcessor(new ConvertInfoReporter()).extractPage(3, skip)
    }
    Assertions.assertResult(true){
      new PDFProcessor(new ConvertInfoReporter()).extractPage(8, skip)
    }
    Assertions.assertResult(true){
      new PDFProcessor(new ConvertInfoReporter()).extractPage(33, skip)
    }
  }
}
