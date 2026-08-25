package monad_core.performance.presentation

import monad_core.performance.application.{NanoClock, PerformanceWorkload}
import monad_core.performance.domain.*
import monad_core.performance.helpers.SequenceNanoClock
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PerformanceRuntimeTest extends AnyFunSuite with Matchers with MockFactory:

  private val Workload = mock[PerformanceWorkload]
  private val Printer  = mock[PerformanceReportPrinter]
  private val Clock    = SequenceNanoClock(Vector(0L, 10L))

  test("the performance runtime runs an experiment and prints its report"):
    val config = PerformanceConfig
      .from(
        startEntities = 100,
        maximumEntities = 100,
        growthFactor = 2,
        iterations = 1,
        warmups = 1,
        frameBudgetNanos = 16L
      )
      .value

    val operation = () => Right(()): Either[PerformanceError, Unit]

    Workload.prepare
      .expects(config.growth.start)
      .returning(Right(operation))
      .once()
    Printer.print
      .expects(*)
      .once()

    val result =
      PerformanceRuntime.handle(ExperimentKind.Load, config)(using Workload, Clock, Printer)

    result shouldBe Right(())

  test("performance runtime should return a PerformanceError if one is encountered"):
    val invalidIterationCount = 0

    val config = PerformanceConfig.default.value

    val error = InvalidIterationCount(invalidIterationCount)

    Workload.prepare
      .expects(*)
      .returning(Left(error))
      .once()

    val result =
      PerformanceRuntime.handle(ExperimentKind.Load, config)(using Workload, Clock, Printer)

    result shouldBe Left(error)

  test("performance runtime should return the first encountered PerformanceError"):
    val invalidIterationCount = 0
    val invalidWarmupCount    = 0

    val config = PerformanceConfig.default.value

    val error1 = InvalidIterationCount(invalidIterationCount)
    val error2 = InvalidWarmupCount(invalidWarmupCount)

    Workload.prepare
      .expects(*)
      .returning(Left(error1))
      .once()
    Workload.prepare
      .expects(*)
      .returning(Left(error2))
      .never()

    val result =
      PerformanceRuntime.handle(ExperimentKind.Load, config)(using Workload, Clock, Printer)

    result shouldBe Left(error1)
