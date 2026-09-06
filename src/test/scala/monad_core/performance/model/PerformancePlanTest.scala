package monad_core.performance.model

import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PerformancePlanTest extends AnyFunSuite with Matchers:

  test("load stores its entity count"):
    val result = PerformancePlan.load(2).value

    result shouldBe PerformancePlan.Load(EntityCount.from(2).value)

  test("load reports the load kind"):
    val result = PerformancePlan.load(2).value

    result.kind shouldBe PerformanceKind.Load

  test("load rejects an invalid entity count"):
    val result = PerformancePlan.load(0)

    result shouldBe Left(InvalidPositiveCount("Entity count", 0))

  test("spike stores its entity range"):
    val range = EntityRange.from(2, 8).value

    val result = PerformancePlan.spike(2, 8).value

    result shouldBe PerformancePlan.Spike(range)

  test("spike reports the spike kind"):
    val result = PerformancePlan.spike(2, 8).value

    result.kind shouldBe PerformanceKind.Spike

  test("spike rejects an invalid entity range"):
    val result = PerformancePlan.spike(8, 2)

    result shouldBe Left(InvalidGrowthMaximum(8, 2))

  test("stress stores its entity growth"):
    val growth = EntityGrowth.from(2, 8, 2).value

    val result = PerformancePlan.stress(2, 8, 2).value

    result shouldBe PerformancePlan.Stress(growth)

  test("stress reports the stress kind"):
    val result = PerformancePlan.stress(2, 8, 2).value

    result.kind shouldBe PerformanceKind.Stress

  test("stress rejects an invalid entity growth"):
    val result = PerformancePlan.stress(2, 8, 1)

    result shouldBe Left(InvalidGrowthFactor(1))

  test("scalability stores its entity growth"):
    val growth = EntityGrowth.from(2, 8, 2).value

    val result = PerformancePlan.scalability(2, 8, 2).value

    result shouldBe PerformancePlan.Scalability(growth)

  test("scalability reports the scalability kind"):
    val result = PerformancePlan.scalability(2, 8, 2).value

    result.kind shouldBe PerformanceKind.Scalability

  test("scalability rejects an invalid entity growth"):
    val result = PerformancePlan.scalability(2, 8, 1)

    result shouldBe Left(InvalidGrowthFactor(1))

  test("the default starting entity count is one hundred"):
    val result = PerformancePlan.DefaultStartEntities

    result shouldBe 100

  test("the default maximum entity count is one thousand six hundred"):
    val result = PerformancePlan.DefaultMaximumEntities

    result shouldBe 1_600

  test("the default growth factor is two"):
    val result = PerformancePlan.DefaultGrowthFactor

    result shouldBe 2
