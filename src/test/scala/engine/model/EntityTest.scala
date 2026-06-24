package engine.model

import engine.model.Shape2D.{Circle, Rectangle}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EntityTest extends AnyFunSuite with Matchers:

  val ValidEntityId = "entity1"
  val ValidPosition = Vector2D(1, 3)

  test("can create an entity with ID, position and the shape of a circle") :
    val radius = 2
    val entity = Entity.circle(ValidEntityId, ValidPosition, radius)

    entity.id shouldBe ValidEntityId
    entity.position.x shouldBe ValidPosition.x
    entity.position.y shouldBe ValidPosition.y
    entity.shape shouldBe Circle(radius)

  test("can create an entity with ID, position and the shape of a rectangle") :
    val height = 2
    val length = 2
    val entity = Entity.rectangle(ValidEntityId, ValidPosition, height, length)

    entity.id shouldBe ValidEntityId
    entity.position.x shouldBe ValidPosition.x
    entity.position.y shouldBe ValidPosition.y
    entity.shape shouldBe Rectangle(height, length)