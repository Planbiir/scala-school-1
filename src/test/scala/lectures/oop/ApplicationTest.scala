package lectures.oop

import org.scalatest.WordSpec
import org.scalatest.{FunSuite, Matchers}

/**
  * Раскомментируйте и допишите тесты на
  * класс lectures.oop.Application
  */
class ApplicationTest extends WordSpec with Matchers{

  private val started = new AfterWord("started")
  val testServ =  new Application(true)
  val realServ =  new Application(false)


  "Application" should {
    "return correct answer" when started{
      "in a test environment" in {
        testServ.doTheJob shouldBe 8
      }
      "in a production environment" in {
         val i = realServ.doTheJob
        i shouldBe 3
      }
    }
  }
}
