package integrations.monad_core.simulator.presentation.performance

import monad_core.performance.simulator.PerformanceCli
import monad_core.simulator.presentation.performance.PerformanceMode
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream

class PerformanceModeTest extends AnyFunSuite with Matchers:

  private val MinimalArguments = Array(
    PerformanceCli.Entities,
    "1",
    PerformanceCli.MaximumEntities,
    "1",
    PerformanceCli.GrowthFactor,
    "2",
    PerformanceCli.Iterations,
    "1",
    PerformanceCli.Warmups,
    "0"
  )

  test("runCommand returns a successful route response"):
    val result = PerformanceMode.runCommand(MinimalArguments, PerformanceCli.LoadRoute)

    result.success shouldBe true

  test("runCommand returns its completion message"):
    val result = PerformanceMode.runCommand(MinimalArguments, PerformanceCli.LoadRoute)

    result.message shouldBe "Finished performance experiment"

  test("runCommand prints a successful report"):
    val output = ByteArrayOutputStream()

    Console.withOut(output):
      PerformanceMode.runCommand(MinimalArguments, PerformanceCli.LoadRoute)

    output.toString should include("Performance experiment: Load")

  test("runCommand returns a failed route response"):
    val result = PerformanceMode.runCommand(Array.empty, "unknown")

    result.success shouldBe false

  test("runCommand returns the performance error message"):
    val result = PerformanceMode.runCommand(Array.empty, "unknown")

    result.message should include("Unknown performance route")
