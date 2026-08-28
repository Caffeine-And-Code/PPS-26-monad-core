package monad_core.performance.domain

import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FrameBudgetTest extends AnyFunSuite with Matchers:

  private val BudgetNanos       = 10L
  private val BelowBudgetNanos  = BudgetNanos - 1L
  private val AboveBudgetNanos  = BudgetNanos + 1L
  private val CompletedRate     = 1.0
  private val NotCompletedRate  = 0.0
  private val HalfCompletedRate = 0.5
  private val Budget            = FrameBudget.from(BudgetNanos).value

  test("a frame budget can be created from a positive value"):
    val value = 1L

    val result = FrameBudget.from(value)

    result.map(_.nanos) shouldBe Right(value)

  test("a frame budget cannot be created from zero"):
    val value = 0L

    val result = FrameBudget.from(value)

    result shouldBe Left(InvalidFrameBudget(value))

  test("a frame budget cannot be created from a negative value"):
    val value = -1L

    val result = FrameBudget.from(value)

    result shouldBe Left(InvalidFrameBudget(value))

  test("completion rate rejects an empty sample collection"):
    val samples = Vector.empty

    val result = Budget.completionRate(samples)

    result shouldBe Left(EmptyPerformanceSamples())

  test("completion rate includes a sample below the frame budget"):
    val samples = Vector(PerformanceSample(BelowBudgetNanos))

    val result = Budget.completionRate(samples)

    result shouldBe Right(CompletedRate)

  test("completion rate includes a sample exactly on the frame budget"):
    val samples = Vector(PerformanceSample(BudgetNanos))

    val result = Budget.completionRate(samples)

    result shouldBe Right(CompletedRate)

  test("completion rate excludes a sample above the frame budget"):
    val samples = Vector(PerformanceSample(AboveBudgetNanos))

    val result = Budget.completionRate(samples)

    result shouldBe Right(NotCompletedRate)

  test("completion rate calculates the fraction of samples within the frame budget"):
    val samples = Vector(
      PerformanceSample(BelowBudgetNanos),
      PerformanceSample(BudgetNanos),
      PerformanceSample(AboveBudgetNanos),
      PerformanceSample(AboveBudgetNanos)
    )

    val result = Budget.completionRate(samples)

    result shouldBe Right(HalfCompletedRate)
