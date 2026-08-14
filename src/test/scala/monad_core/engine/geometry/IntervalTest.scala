package monad_core.engine.geometry

import monad_core.engine.geometry.Interval
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.*

class IntervalTest extends AnyFunSuite with Inside with Matchers with MockFactory:

  test("an Interval contains a value"):
    val cases = Table(
      ("min", "max", "value"),
      (0, 10, 0),
      (0, 10, 10),
      (0, 10, 5)
    )

    forAll(cases): (min, max, value) =>
      val interval: Interval = Interval(min, max)

      val result = interval contains value

      result shouldBe true

  test("an Interval does not contains values outside of it"):
    val cases = Table(
      ("min", "max", "value"),
      (0, 10, -1),
      (0, 10, 11),
      (0, 10, 20)
    )

    forAll(cases): (min, max, value) =>
      val interval: Interval = Interval(min, max)

      val result = interval contains value

      result shouldBe false
