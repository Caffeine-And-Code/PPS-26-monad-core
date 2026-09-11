package monad_core.simulator.application.logging

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LoggerTest extends AnyFunSuite with Matchers:

  test("can implement a Logger"):
    val message       = "message"
    var loggedMessage = Option.empty[String]
    val logger = new Logger:
      override def info(message: String): Unit =
        loggedMessage = Some(message)

    logger.info(message)

    loggedMessage shouldBe Some(message)

  test("trace does nothing when it is not overridden"):
    var loggedMessage = Option.empty[String]
    val logger = new Logger:
      override def info(message: String): Unit =
        loggedMessage = Some(message)

    logger.trace("message")

    loggedMessage shouldBe None
