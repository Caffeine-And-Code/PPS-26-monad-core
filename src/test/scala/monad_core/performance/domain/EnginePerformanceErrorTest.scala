package monad_core.performance.domain

import monad_core.engine.model.WorldBoundsCannotBeNegativeOrZero
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EnginePerformanceErrorTest extends AnyFunSuite with Matchers:

  test("an engine performance error adds workload context to its cause"):
    val cause           = WorldBoundsCannotBeNegativeOrZero()
    val expectedMessage = s"Engine workload failed: ${cause.message}"

    val result = EnginePerformanceError(cause)

    result.message shouldBe expectedMessage
