package engine.model

import engine.model.Shape2D.{Circle, Rectangle}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EntityTest extends AnyFunSuite with Matchers:

  val ValidEntityId = "entity1"
  val ValidPosition = Vector2D(1, 3)
  val ValidRadius = 2;
  val ValidHeight = 2;
  val ValidLength = 2;

  test("can create an entity with ID, position and the shape of a circle") :
    val entity = Entity.circle(ValidEntityId, ValidPosition, ValidRadius)

    entity.id shouldBe ValidEntityId
    entity.position.x shouldBe ValidPosition.x
    entity.position.y shouldBe ValidPosition.y
    entity.shape shouldBe Circle(ValidRadius)

  test("can create an entity with ID, position and the shape of a rectangle") :
    val height = 2
    val length = 2
    val entity = Entity.rectangle(ValidEntityId, ValidPosition, ValidHeight, ValidLength)

    entity.id shouldBe ValidEntityId
    entity.position.x shouldBe ValidPosition.x
    entity.position.y shouldBe ValidPosition.y
    entity.shape shouldBe Rectangle(ValidHeight, ValidLength)

  test("cannot create an entity with an empty ID"):
    val invalidEntityId = "    "
    an [IllegalArgumentException] shouldBe thrownBy:
      Entity(invalidEntityId, ValidPosition, Shape2D.circle(ValidRadius))

  test("cannot create an entity with an invalid position"):
    val invalidPositionX = Vector2D(-1, 1)
    val invalidPositionY = Vector2D(1, -1)
    val invalidPositionXY = Vector2D(1, 1)

    an[IllegalArgumentException] shouldBe thrownBy:
      Entity(ValidEntityId, invalidPositionX, Shape2D.circle(ValidRadius))
    an[IllegalArgumentException] shouldBe thrownBy:
      Entity(ValidEntityId, invalidPositionY, Shape2D.circle(ValidRadius))
      an[IllegalArgumentException] shouldBe thrownBy:
        Entity(ValidEntityId, invalidPositionXY, Shape2D.circle(ValidRadius))