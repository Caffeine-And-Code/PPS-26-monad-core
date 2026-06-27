package engine.collision_detection

import engine.collision_detection.Containing.isInside
import engine.model.*
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ContainingTest extends AnyFunSuite with Inside with Matchers:


  test("can detect if a circular entity is over a circular surface"):
    val eitherEntity = Entity.circle("en1", Vector2D(0, 0), 1)
    val eitherSurface = Surface.circle("sur1", Vector2D(3, 3), 10)

    val isEntityOverSurface = for {
      entity <- eitherEntity
      surface <- eitherSurface
    } yield entity isInside surface

    inside(isEntityOverSurface):
      case Right(value) => value shouldBe true