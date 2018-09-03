package pl.mtpl.tools.ebooks

import org.scalatest.{Assertions, FlatSpec, Matchers}

class ReplaceCarryOversL0Test extends FlatSpec with Matchers {
  "Clean up book" should "remove all one line carry-overs" in {
    Assertions.assertResult("This is reassigned text that contains no carry-over words left after PDF formatted pages."){
      new PDFtoEPUBCConverter(null, null).cleanUp("This is reassi- gned text that cont- ains no carry-over words left after PDF for- matted pages.")
    }
  }

  "Clean up book" should "remove all two lines carry-overs" in {
    Assertions.assertResult("This is reassigned text that contains no carry-over words left after PDF formatted pages."){
      new PDFtoEPUBCConverter(null, null).cleanUp(
        """This is reassi-
          |gned text that cont-
          | ains no carry-over words left after PDF for-
          | matted pages.""".stripMargin)
    }
  }
}
