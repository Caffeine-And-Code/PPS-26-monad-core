package monad_core.performance.model

import monad_core.engine.physics.core.NegativeDeltaTime
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PerformanceErrorTest extends AnyFunSuite with Matchers:

  test("InvalidPositiveCount describes the rejected count"):
    val result = InvalidPositiveCount("Samples", 0)

    result.message shouldBe "Samples must be positive: 0"

  test("InvalidWarmupCount describes the rejected count"):
    val result = InvalidWarmupCount(-1)

    result.message shouldBe "Warm-up count cannot be negative: -1"

  test("InvalidGrowthFactor describes the rejected factor"):
    val result = InvalidGrowthFactor(1)

    result.message shouldBe "Growth factor must be greater than one: 1"

  test("InvalidGrowthMaximum describes both growth bounds"):
    val result = InvalidGrowthMaximum(10, 5)

    result.message shouldBe "Maximum entity count 5 cannot be lower than start 10"

  test("InvalidFrameBudget describes the rejected budget"):
    val result = InvalidFrameBudget(0L)

    result.message shouldBe "Frame budget must be positive: 0"

  test("InvalidPerformanceArgument describes its name and value"):
    val result = InvalidPerformanceArgument("--entities", "many")

    result.message shouldBe "Invalid value 'many' for argument '--entities'"

  test("UnknownPerformanceRoute describes the unsupported route"):
    val result = UnknownPerformanceRoute("unknown")

    result.message shouldBe "Unknown performance route: unknown"

  test("EmptyPerformanceSamples describes the missing samples"):
    val result = EmptyPerformanceSamples()

    result.message shouldBe "At least one performance sample is required"

  test("EnginePerformanceError includes the engine error message"):
    val result = EnginePerformanceError(NegativeDeltaTime(-1L))

    result.message shouldBe "Engine workload failed: Delta time cannot be negative: -1"
