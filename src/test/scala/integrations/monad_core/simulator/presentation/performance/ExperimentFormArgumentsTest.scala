package integrations.monad_core.simulator.presentation.performance

import monad_core.performance.presentation.{PerformanceArguments, PerformanceRoutes}
import monad_core.simulator.application.performance.ExperimentRequest
import monad_core.simulator.domain.performance.{
  MissingPerformanceArgument,
  UnknownPerformanceExperimentType
}
import monad_core.simulator.presentation.performance.ExperimentFormArguments
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ExperimentFormArgumentsTest extends AnyFunSuite with Matchers:

  private val CommonValues = Map(
    ExperimentFormArguments.PerformanceExperimentType -> "Load",
    PerformanceArguments.Entities                     -> "100",
    PerformanceArguments.Iterations                   -> "20",
    PerformanceArguments.Warmups                      -> "5",
    PerformanceArguments.FrameBudgetMillis            -> "16"
  )

  private val MaximumEntitiesValue = Map(
    PerformanceArguments.MaximumEntities              -> "1600"
  )

  private val GrowthFactorValue = Map(
    PerformanceArguments.GrowthFactor -> "2"
  )

  private val CommonArguments = Vector(
    PerformanceArguments.Entities,
    "100",
    PerformanceArguments.Iterations,
    "20",
    PerformanceArguments.Warmups,
    "5",
    PerformanceArguments.FrameBudgetMillis,
    "16"
  )

  private val MaximumEntitiesArgument = Vector(
    PerformanceArguments.MaximumEntities,
    "1600"
  )

  private val GrowthFactorArgument = Vector(
    PerformanceArguments.GrowthFactor,
    "2"
  )

  test("load form arguments contain every editable common parameter"):
    val result = ExperimentFormArguments.from(CommonValues)

    result shouldBe Right(
      ExperimentRequest(PerformanceRoutes.Load, CommonArguments)
    )

  test("stress form arguments add maximum entities and growth factor"):
    val values = CommonValues ++ MaximumEntitiesValue ++ GrowthFactorValue
      .updated(ExperimentFormArguments.PerformanceExperimentType, "Stress")

    val result = ExperimentFormArguments.from(values)

    result shouldBe Right(
      ExperimentRequest(
        PerformanceRoutes.Stress,
        CommonArguments ++
          MaximumEntitiesArgument ++
            GrowthFactorArgument
      )
    )

  test("spike form arguments add only maximum entities"):
    val values = CommonValues ++ MaximumEntitiesValue
      .updated(ExperimentFormArguments.PerformanceExperimentType, "Spike")

    val result = ExperimentFormArguments.from(values)

    result shouldBe Right(
      ExperimentRequest(
        PerformanceRoutes.Spike,
        CommonArguments ++ MaximumEntitiesArgument
      )
    )

  test("scalability form arguments add maximum entities and growth factor"):
    val values = CommonValues ++ MaximumEntitiesValue ++ GrowthFactorValue
      .updated(ExperimentFormArguments.PerformanceExperimentType, "Scalability")

    val result = ExperimentFormArguments.from(values)

    result shouldBe Right(
      ExperimentRequest(
        PerformanceRoutes.Scalability,
        CommonArguments ++
          MaximumEntitiesArgument ++
            GrowthFactorArgument
      )
    )

  test("form arguments reject a missing test type"):
    val values = CommonValues - ExperimentFormArguments.PerformanceExperimentType

    val result = ExperimentFormArguments.from(values)

    result shouldBe Left(
      MissingPerformanceArgument(ExperimentFormArguments.PerformanceExperimentType)
    )

  test("form arguments reject an unknown test type"):
    val unknownLabel = "Unknown"
    val values =
      CommonValues.updated(ExperimentFormArguments.PerformanceExperimentType, unknownLabel)

    val result = ExperimentFormArguments.from(values)

    result shouldBe Left(UnknownPerformanceExperimentType(unknownLabel))
