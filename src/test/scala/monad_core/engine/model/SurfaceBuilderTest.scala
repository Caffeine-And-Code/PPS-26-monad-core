package monad_core.engine.model

import monad_core.engine.model.SurfaceBuilder.*
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SurfaceBuilderTest extends AnyFunSuite with Inside with Matchers:

  val ValidSurfaceId          = "surface1"
  val ValidPosition: Vector2D = Vector2D(1, 3)
  val ValidRadius             = 2

  val ValidSurface: Either[EngineError, Surface] =
    Surface.circle(ValidSurfaceId, ValidPosition, ValidRadius)

  test("can build a surface with all optional properties"):
    val frictionIndex = 0.5
    val appliedForce  = Vector2D(3, 4)
    val damage        = 2

    val surface = ValidSurface
      .withFrictionIndex(Some(frictionIndex))
      .withAppliedForce(Some(appliedForce))
      .withDamageOverTime(Some(damage))

    inside(surface):
      case Right(surface) =>
        surface.frictionIndex shouldBe Some(frictionIndex)
        surface.appliedForce shouldBe Some(appliedForce)
        surface.damageOverTime.map(_.value) shouldBe Some(damage)

  test("can build a surface without optional properties"):
    val surface = ValidSurface
      .withFrictionIndex(None)
      .withAppliedForce(None)
      .withDamageOverTime(None)

    inside(surface):
      case Right(surface) =>
        surface.frictionIndex shouldBe None
        surface.appliedForce shouldBe None
        surface.damageOverTime shouldBe None

  test("cannot build a surface with invalid damage over time"):
    val invalidDamage = -1

    val surface = ValidSurface.withDamageOverTime(Some(invalidDamage))

    surface shouldBe Left(DamageCannotBeNegative())

  test("a surface builder preserves a previous error"):
    val invalidSurface = Surface.circle("", ValidPosition, ValidRadius)

    val surface = invalidSurface.withFrictionIndex(Some(0.5))

    surface shouldBe Left(LocatableIdCannotBeEmpty())
