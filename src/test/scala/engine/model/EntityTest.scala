package engine.model

import engine.model.Shape2D.Circle
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EntityTest extends AnyFunSuite with Matchers:

  test("can create an entity with ID, position and the shape of a circle") :
    val entityId = "circle1"
    val position = Vector2D(1, 3)
    val radius = 2
    val entity = Entity.circle(entityId, position, radius)

    entity.id shouldBe entityId
    entity.position.x shouldBe position.x
    entity.position.y shouldBe position.y
    entity.shape shouldBe Circle(2)