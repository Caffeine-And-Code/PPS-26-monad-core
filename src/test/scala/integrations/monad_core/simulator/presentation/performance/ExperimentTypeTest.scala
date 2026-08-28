package integrations.monad_core.simulator.presentation.performance

import monad_core.performance.presentation.{PerformanceArguments, PerformanceRoutes}
import monad_core.simulator.domain.performance.UnknownPerformanceExperimentType
import monad_core.simulator.presentation.performance.ExperimentType
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ExperimentTypeTest extends AnyFunSuite with Matchers:

  test("load selects its route without additional parameters"):
    val result = ExperimentType.fromLabel("Load")

    val resultValue = result.map(testType => (testType.route, testType.specificArguments))

    resultValue shouldBe Right(
      (PerformanceRoutes.Load, Vector.empty)
    )

  test("stress selects its route with maximum entities and growth factor"):
    val result = ExperimentType.fromLabel("Stress")

    val resultValue = result.map(testType => (testType.route, testType.specificArguments))

    resultValue shouldBe Right(
      (
        PerformanceRoutes.Stress,
        Vector(
          PerformanceArguments.MaximumEntities,
          PerformanceArguments.GrowthFactor
        )
      )
    )

  test("spike selects its route with maximum entities"):
    val result = ExperimentType.fromLabel("Spike")

    val resultValue = result.map(testType => (testType.route, testType.specificArguments))

    resultValue shouldBe Right(
      (
        PerformanceRoutes.Spike,
        Vector(PerformanceArguments.MaximumEntities)
      )
    )

  test("scalability selects its route with maximum entities and growth factor"):
    val result = ExperimentType.fromLabel("Scalability")

    val resultValue = result.map(testType => (testType.route, testType.specificArguments))

    resultValue shouldBe Right(
      (
        PerformanceRoutes.Scalability,
        Vector(
          PerformanceArguments.MaximumEntities,
          PerformanceArguments.GrowthFactor
        )
      )
    )

  test("an unsupported test-type label is rejected"):
    val unsupportedLabel = "Unsupported"

    val result = ExperimentType.fromLabel(unsupportedLabel)

    result shouldBe Left(UnknownPerformanceExperimentType(unsupportedLabel))
