package engine.model

import engine.model.Shape2D.{Circle, Rectangle}
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SurfaceTest extends AnyFunSuite with Matchers with Inside :

  val ValidEntityId = "entity1"
  val ValidPosition = Vector2D(1, 3)
  val ValidRadius = 2
  val ValidHeight = 2
  val ValidLength = 2

  val ValidSurface: Either[String, Surface] = Surface.circle(ValidEntityId, ValidPosition, ValidRadius)

  test("can create a surface with a circle shape"):
    val entity = Surface.circle(ValidEntityId, ValidPosition, ValidRadius)

    inside(entity):
      case Right(surface) =>
        surface.id.value shouldBe ValidEntityId
        surface.position shouldBe ValidPosition
        surface.shape shouldBe Circle(ValidRadius)

  test("can create a surface with a rectangle shape"):
    val entity = Surface.rectangle(ValidEntityId, ValidPosition, ValidHeight, ValidLength)

    inside(entity):
      case Right(surface) =>
        surface.id.value shouldBe ValidEntityId
        surface.position shouldBe ValidPosition
        surface.shape shouldBe Rectangle(ValidHeight, ValidLength)

  test("can create a surface and give it an friction index"):
    val frictionIndex = 2

    val surfaceWithFrictionIndex = ValidSurface.flatMap(_.withFrictionIndex(frictionIndex))

    inside(surfaceWithFrictionIndex) :
      case Right(surface) => surface.frictionIndex shouldBe frictionIndex

  test("can create a surface and give it a negative friction index"):
    val frictionIndex = -2

    val surfaceWithFrictionIndex = ValidSurface.flatMap(_.withFrictionIndex(frictionIndex))

    inside(surfaceWithFrictionIndex) :
      case Right(surface) => surface.frictionIndex shouldBe frictionIndex

  test("can create a surface and give it an valid applied force"):
    val appliedForce = Vector2D(10, 20)

    val surfaceWithAppliedForce = ValidSurface.flatMap(_.withAppliedForce(appliedForce))

    inside(surfaceWithAppliedForce) :
      case Right(surface) => surface.appliedForce shouldBe appliedForce

  test("can create a surface and give it a negative applied force"):
    val appliedForce = Vector2D(-10, -20)

    val surfaceWithAppliedForce = ValidSurface.flatMap(_.withAppliedForce(appliedForce))

    inside(surfaceWithAppliedForce):
      case Right(surface) => surface.appliedForce shouldBe appliedForce
