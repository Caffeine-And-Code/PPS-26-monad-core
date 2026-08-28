package monad_core.performance.application

import monad_core.performance.domain.*
import monad_core.performance.helpers.SequenceNanoClock
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SampleCollectorTest extends AnyFunSuite with Matchers:

  private val ExpectedError = InvalidPerformanceArgument("operation", "failure")

  test("the sample collector executes every requested warm-up"):
    val warmups    = WarmupCount.from(2).value
    var executions = 0
    val operation = () =>
      executions += 1
      Right(())

    val result = SampleCollector.warmUp(warmups, operation)

    result shouldBe Right(())
    executions shouldBe warmups.value

  test("the sample collector executes no warm-ups when none are requested"):
    val warmups    = WarmupCount.from(0).value
    var executions = 0
    val operation = () =>
      executions += 1
      Right(())

    val result = SampleCollector.warmUp(warmups, operation)

    result shouldBe Right(())
    executions shouldBe 0

  test("the sample collector propagates an error raised during warm-up"):
    val warmups   = WarmupCount.from(1).value
    val operation = () => Left(ExpectedError)

    val result = SampleCollector.warmUp(warmups, operation)

    result shouldBe Left(ExpectedError)

  test("the sample collector stops warm-up at the first error"):
    val warmups    = WarmupCount.from(2).value
    var executions = 0
    val operation = () =>
      executions += 1
      Left(ExpectedError)

    SampleCollector.warmUp(warmups, operation)

    executions shouldBe 1

  test("the sample collector measures every requested iteration"):
    val clock      = SequenceNanoClock(Vector(10L, 15L, 20L, 28L))
    val iterations = IterationCount.from(2).value
    val operation  = () => Right(()): Either[PerformanceError, Unit]

    val result = SampleCollector.collect(iterations, operation)(using clock)

    result shouldBe Right(Vector(PerformanceSample(5L), PerformanceSample(8L)))

  test("the sample collector executes every requested measured iteration"):
    val clock      = SequenceNanoClock(Vector(10L, 15L, 20L, 28L))
    val iterations = IterationCount.from(2).value
    var executions = 0
    val operation = () =>
      executions += 1
      Right(())

    SampleCollector.collect(iterations, operation)(using clock)

    executions shouldBe iterations.value

  test("the sample collector propagates an error raised during collection"):
    val clock      = SequenceNanoClock(Vector(10L))
    val iterations = IterationCount.from(1).value
    val operation  = () => Left(ExpectedError)

    val result = SampleCollector.collect(iterations, operation)(using clock)

    result shouldBe Left(ExpectedError)

  test("the sample collector stops collection at the first error"):
    val clock      = SequenceNanoClock(Vector(10L))
    val iterations = IterationCount.from(2).value
    var executions = 0
    val operation = () =>
      executions += 1
      Left(ExpectedError)

    SampleCollector.collect(iterations, operation)(using clock)

    executions shouldBe 1
