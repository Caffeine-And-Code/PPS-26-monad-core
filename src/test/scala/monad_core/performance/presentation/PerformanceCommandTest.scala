package monad_core.performance.presentation

import monad_core.performance.application.{PerformanceWorkload, SampleCollector}
import monad_core.performance.domain.*
import monad_core.performance.helpers.SequenceNanoClock
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PerformanceCommandTest extends AnyFunSuite with Matchers:

  private val ValidArguments = Array(
    PerformanceArguments.Entities,
    "1",
    PerformanceArguments.MaximumEntities,
    "1",
    PerformanceArguments.GrowthFactor,
    "2",
    PerformanceArguments.Iterations,
    "1",
    PerformanceArguments.Warmups,
    "0",
    PerformanceArguments.FrameBudgetMillis,
    "16"
  )

  private val SuccessfulWorkload = new PerformanceWorkload:
    override def prepare(
        entityCount: EntityCount
    ): Either[PerformanceError, SampleCollector.Operation] =
      Right(() => Right(()))

  private def run(route: String): Either[PerformanceError, ExperimentReport] =
    val clock = SequenceNanoClock(Vector.tabulate(20)(_.toLong))
    PerformanceCommand.run(route, ValidArguments)(using SuccessfulWorkload, clock)

  test("the performance command routes a load command"):
    val result = run(PerformanceRoutes.Load)

    result.map(_.kind) shouldBe Right(ExperimentKind.Load)

  test("the performance command routes a stress command"):
    val result = run(PerformanceRoutes.Stress)

    result.map(_.kind) shouldBe Right(ExperimentKind.Stress)

  test("the performance command routes a spike command"):
    val result = run(PerformanceRoutes.Spike)

    result.map(_.kind) shouldBe Right(ExperimentKind.Spike)

  test("the performance command routes a scalability command"):
    val result = run(PerformanceRoutes.Scalability)

    result.map(_.kind) shouldBe Right(ExperimentKind.Scalability)

  test("the performance command rejects an unknown route"):
    val unknownRoute = "unknown-performance-test"

    val result = run(unknownRoute)

    result shouldBe Left(UnknownPerformanceRoute(unknownRoute))

  test("an unknown performance route identifies the rejected command"):
    val unknownRoute = "unknown-performance-test"

    val error = UnknownPerformanceRoute(unknownRoute)

    error.message should include(unknownRoute)

  test("the performance command returns an argument parsing error"):
    val invalidValue = "invalid"
    val arguments    = Array(PerformanceArguments.Entities, invalidValue)
    val clock        = SequenceNanoClock(Vector.empty)

    val result = PerformanceCommand.run(PerformanceRoutes.Stress, arguments)(using
      SuccessfulWorkload,
      clock
    )

    result shouldBe Left(
      InvalidPerformanceArgument(PerformanceArguments.Entities, invalidValue)
    )
