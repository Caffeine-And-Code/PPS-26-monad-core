package integrations.monad_core.engine.collision_detection

import monad_core.engine.collision_detection.Containing.isInside
import monad_core.engine.geometry.ShapeCollision.shapeCollidesWithShape
import monad_core.engine.geometry.ShapeContainment.given
import monad_core.engine.model.*
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ContainingTest extends AnyFunSuite with Inside with Matchers:

  test("can detect if a circular entity is inside a circular surface"):
    val eitherEntity  = Entity.circle("en1", Vector2D(0, 0), 1)
    val eitherSurface = Surface.circle("sur1", Vector2D(3, 3), 10)

    val isEntityOverSurface = for
      entity  <- eitherEntity
      surface <- eitherSurface
    yield entity isInside surface

    inside(isEntityOverSurface):
      case Right(value) => value shouldBe true

  test("can detect if a rectangular entity is inside a circular surface"):
    val eitherEntity  = Entity.rectangle("en1", Vector2D(0, 0), 1, 1)
    val eitherSurface = Surface.circle("sur1", Vector2D(3, 3), 10)

    val isEntityOverSurface = for
      entity  <- eitherEntity
      surface <- eitherSurface
    yield entity isInside surface

    inside(isEntityOverSurface):
      case Right(value) => value shouldBe true

  test("can detect if an entity is not inside a circular surface"):
    val eitherEntity  = Entity.circle("en1", Vector2D(0, 0), 1)
    val eitherSurface = Surface.circle("sur1", Vector2D(3, 3), 1)

    val isEntityOverSurface = for
      entity  <- eitherEntity
      surface <- eitherSurface
    yield entity isInside surface

    inside(isEntityOverSurface):
      case Right(value) => value shouldBe false

  test("can detect if an entity is inside a rectangular surface"):
    val eitherEntity  = Entity.circle("en1", Vector2D(0, 0), 1)
    val eitherSurface = Surface.rectangle("sur1", Vector2D(3, 3), 7, 9)

    val isEntityOverSurface = for
      entity  <- eitherEntity
      surface <- eitherSurface
    yield entity isInside surface

    inside(isEntityOverSurface):
      case Right(value) => value shouldBe true

  test("can detect if an entity is not inside a rectangular surface"):
    val eitherEntity  = Entity.circle("en1", Vector2D(8, 0), 1)
    val eitherSurface = Surface.rectangle("sur1", Vector2D(3, 3), 7, 9)

    val isEntityOverSurface = for
      entity  <- eitherEntity
      surface <- eitherSurface
    yield entity isInside surface

    inside(isEntityOverSurface):
      case Right(value) => value shouldBe false

  test("an entity is inside another entity only if his center is inside"):
    val eitherEntity  = Entity.circle("en1", Vector2D(7.5, 0), 1)
    val eitherSurface = Surface.rectangle("sur1", Vector2D(3, 3), 8, 9)

    val isEntityOverSurface = for
      entity  <- eitherEntity
      surface <- eitherSurface
    yield entity isInside surface

    inside(isEntityOverSurface):
      case Right(value) => value shouldBe true

  test("an entity is not inside another entity only if his center is not inside"):
    val eitherEntity  = Entity.circle("en1", Vector2D(7.6, 0), 1)
    val eitherSurface = Surface.rectangle("sur1", Vector2D(3, 3), 7, 9)

    val isEntityOverSurface = for
      entity  <- eitherEntity
      surface <- eitherSurface
    yield entity isInside surface

    inside(isEntityOverSurface):
      case Right(value) => value shouldBe false

  test("isInside considers the rotation of a rectangular surface"):
    val eitherSurface = Surface.rectangle("sur1", Vector2D(5, 5), 2, 8, 90)
    val insideEntity  = Entity.circle("inside", Vector2D(5, 8.9), 1)
    val outsideEntity = Entity.circle("outside", Vector2D(8.9, 5), 1)

    val results = for
      surface <- eitherSurface
      inside  <- insideEntity
      outside <- outsideEntity
    yield (inside.isInside(surface), outside.isInside(surface))

    results shouldBe Right((true, false))
