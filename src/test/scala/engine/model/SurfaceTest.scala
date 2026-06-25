package engine.model

import engine.model.Shape2D.{Circle, Rectangle}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SurfaceTest extends AnyFunSuite with Matchers :

  val ValidEntityId = "entity1"
  val ValidPosition = Vector2D(1, 3)
  val ValidRadius = 2
  val ValidHeight = 2
  val ValidLength = 2

  test("can create a surface with a circle shape"):
    val entity = Surface.circle(ValidEntityId, ValidPosition, ValidRadius)

    entity shouldBe Right(Surface(ValidEntityId, ValidPosition, Circle(ValidRadius)))

  test("can create a surface with a rectangle shape"):
    val entity = Surface.rectangle(ValidEntityId, ValidPosition, ValidHeight, ValidLength)

    entity shouldBe Right(Surface(ValidEntityId, ValidPosition, Rectangle(ValidHeight, ValidLength)))