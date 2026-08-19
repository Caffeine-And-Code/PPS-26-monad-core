package monad_core.simulator.infrastructure.logging

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

class ConsoleLoggerTest extends AnyFunSuite with Matchers:

  test("can log an info message to console"):
    val message = "Evaluation test completed"
    val output  = ByteArrayOutputStream()

    Console.withOut(output):
      ConsoleLogger.info(message)

    output.toString(StandardCharsets.UTF_8) shouldBe s"$message${System.lineSeparator()}"
