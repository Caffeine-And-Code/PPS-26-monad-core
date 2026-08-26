package monad_core.engine.model

import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.model.{Shape2D, Surface, Vector2D}
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SurfaceTest extends AnyFunSuite with Matchers with Inside:

  val ValidEntityId           = "entity1"
  val ValidPosition: Vector2D = Vector2D(1, 3)
  val ValidRadius             = 2
  val ValidHeight             = 2
  val ValidLength             = 2

  val ValidSurface: Either[EngineError, Surface] =
    Surface.circle(ValidEntityId, ValidPosition, ValidRadius)

  test("can create a surface with a circle shape"):
    val entity = Surface.circle(ValidEntityId, ValidPosition, ValidRadius)

    inside(entity):
      case Right(surface) =>
        surface.id.value shouldBe ValidEntityId
        surface.position shouldBe ValidPosition
        surface.shape shouldBe Shape2D.circle(ValidRadius).toOption.get
        surface.rotation shouldBe 0
        surface.frictionIndex shouldBe None
        surface.appliedForce shouldBe None
        surface.damageOverTime shouldBe None

  test("can create a surface with a rectangle shape"):
    val entity = Surface.rectangle(ValidEntityId, ValidPosition, ValidHeight, ValidLength)

    inside(entity):
      case Right(surface) =>
        surface.id.value shouldBe ValidEntityId
        surface.position shouldBe ValidPosition
        surface.shape shouldBe Shape2D.rectangle(ValidHeight, ValidLength).toOption.get
        surface.frictionIndex shouldBe None
        surface.appliedForce shouldBe None
        surface.damageOverTime shouldBe None

  test("can create a surface and give it an friction index"):
    val frictionIndex = 2

    val surfaceWithFrictionIndex = ValidSurface.flatMap(_.withFrictionIndex(frictionIndex))

    inside(surfaceWithFrictionIndex):
      case Right(surface) => surface.frictionIndex shouldBe Some(frictionIndex)

  test("can create a surface and give it a negative friction index"):
    val frictionIndex = -2

    val surfaceWithFrictionIndex = ValidSurface.flatMap(_.withFrictionIndex(frictionIndex))

    inside(surfaceWithFrictionIndex):
      case Right(surface) => surface.frictionIndex shouldBe Some(frictionIndex)

  test("can create a surface and give it an valid applied force"):
    val appliedForce = Vector2D(10, 20)

    val surfaceWithAppliedForce = ValidSurface.flatMap(_.withAppliedForce(appliedForce))

    inside(surfaceWithAppliedForce):
      case Right(surface) => surface.appliedForce shouldBe Some(appliedForce)

  test("can create a surface and give it a negative applied force"):
    val appliedForce = Vector2D(-10, -20)

    val surfaceWithAppliedForce = ValidSurface.flatMap(_.withAppliedForce(appliedForce))

    inside(surfaceWithAppliedForce):
      case Right(surface) => surface.appliedForce shouldBe Some(appliedForce)

  test("can create a surface and give it damage over time"):
    val damage = 5

    val surfaceWithDamage = ValidSurface.flatMap(_.withDamageOverTime(damage))

    inside(surfaceWithDamage):
      case Right(surface) => surface.damageOverTime.map(_.value) shouldBe Some(damage)

  test("cannot create a surface and give it negative damage over time"):
    val invalidDamage = -1

    val surfaceWithDamage = ValidSurface.flatMap(_.withDamageOverTime(invalidDamage))

    surfaceWithDamage shouldBe Left(DamageCannotBeNegative())
