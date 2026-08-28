package monad_core.performance.domain

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.matchers.should.Matchers

class EntityGrowthTest extends AnyFunSuite with Matchers:

  private val Start   = 100
  private val Maximum = 800
  private val Factor  = 2

  test("entity growth produces geometrically increasing entity counts up to the maximum"):
    val growth = EntityGrowth.from(start = Start, maximum = Maximum, factor = Factor).value

    val result = growth.counts

    result.map(_.map(_.value)) shouldBe Right(Vector(100, 200, 400, 800))

  test("entity growth rejects a factor that cannot increase the load (= 1)"):
    val invalidFactor = 1

    val result = EntityGrowth.from(start = Start, maximum = Maximum, factor = invalidFactor)

    result shouldBe Left(InvalidGrowthFactor(invalidFactor))

  test("entity growth rejects a maximum entity count lower than start"):
    val invalidMaximum = 50

    val result = EntityGrowth.from(start = Start, maximum = invalidMaximum, factor = Factor)

    result shouldBe Left(InvalidGrowthMaximum(Start, invalidMaximum))
