package monad_core.engine.physics.utils

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.*
import org.scalatest.prop.Tables.Table

class RotationTest extends AnyFunSuite with Matchers:
  test("normalize should return the equivalent rotation between 0 and 360 degrees"):
    val cases = Table(
      ("rotation", "expected"),
      (45.0, 45.0),
      (-45.0, 315.0),
      (405.0, 45.0)
    )

    forAll(cases): (rotation, expected) =>
      Rotation.normalize(rotation) shouldBe expected

  test("interpolate should return the interpolated rotation between two rotations"):
    val cases = Table(
      ("previous", "next", "alpha", "expected"),
      (0.0, 90.0, 0.5, 45.0),
      (90.0, 0.0, 0.5, 45.0),
      (350.0, 10.0, 0.5, 0.0),
      (10.0, 350.0, 0.5, 0.0)
    )

    forAll(cases): (previous, next, alpha, expected) =>
      Rotation.interpolate(previous, next, alpha) shouldBe expected
