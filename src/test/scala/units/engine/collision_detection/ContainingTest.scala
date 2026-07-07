package units.engine.collision_detection

import engine.collision_detection.Containing.isInside
import engine.geometry.Placed.placed
import engine.geometry.{Contains, Placed}
import engine.model.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.*
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ContainingTest extends AnyFunSuite with Inside with Matchers with MockFactory:

  test("isInside returns true if the results produced by Contains is true"):
    val entity = Entity.circle("en1", Vector2D(10, 20), 1).value
    val surface = Surface.rectangle("sur1", Vector2D(3, 4), 7, 9).value
    val containsInstance = mock[Contains[Shape2D]]

    containsInstance.contains
      .expects(surface.placed, entity.position)
      .returning(true)
      .once()

    val result = entity.isInside(surface)(using containsInstance)

    result shouldBe true

  test("isInside returns false if the results produced by Contains is false"):
    val entity = Entity.circle("en1", Vector2D(10, 20), 1).value
    val surface = Surface.circle("sur1", Vector2D(3, 4), 5).value
    val containsInstance = mock[Contains[Shape2D]]

    containsInstance.contains
      .expects(surface.placed, entity.position)
      .returning(false)
      .once()

    val result = entity.isInside(surface)(using containsInstance)

    result shouldBe false
