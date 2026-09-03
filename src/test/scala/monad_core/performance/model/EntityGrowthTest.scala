package monad_core.performance.model

import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EntityGrowthTest extends AnyFunSuite with Matchers:

  private def growth(start: Int, maximum: Int, factor: Int): EntityGrowth =
    EntityGrowth.from(start, maximum, factor).value

  private def countValues(entityGrowth: EntityGrowth): Vector[Int] =
    entityGrowth.counts.value.map(_.value)

  test("GrowthFactor accepts a value greater than one"):
    val result = GrowthFactor.from(2)

    result.value.value shouldBe 2

  test("GrowthFactor rejects one"):
    val result = GrowthFactor.from(1)

    result shouldBe Left(InvalidGrowthFactor(1))

  test("GrowthFactor rejects zero"):
    val result = GrowthFactor.from(0)

    result shouldBe Left(InvalidGrowthFactor(0))

  test("EntityGrowth rejects an invalid start count"):
    val result = EntityGrowth.from(0, 10, 2)

    result shouldBe Left(InvalidPositiveCount("Entity count", 0))

  test("EntityGrowth rejects an invalid maximum count"):
    val result = EntityGrowth.from(1, 0, 2)

    result shouldBe Left(InvalidPositiveCount("Entity count", 0))

  test("EntityGrowth rejects a maximum lower than its start"):
    val result = EntityGrowth.from(10, 5, 2)

    result shouldBe Left(InvalidGrowthMaximum(10, 5))

  test("EntityGrowth rejects an invalid growth factor"):
    val result = EntityGrowth.from(1, 10, 1)

    result shouldBe Left(InvalidGrowthFactor(1))

  test("counts contains only the start when start equals maximum"):
    val result = countValues(growth(4, 4, 2))

    result shouldBe Vector(4)

  test("counts multiplies each count by the growth factor"):
    val result = countValues(growth(2, 16, 2))

    result shouldBe Vector(2, 4, 8, 16)

  test("counts caps the last count at the configured maximum"):
    val result = countValues(growth(3, 10, 2))

    result shouldBe Vector(3, 6, 10)
