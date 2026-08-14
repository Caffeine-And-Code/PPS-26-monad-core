package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Vector2D
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PointOnCircleTest extends AnyFunSuite with Matchers :
  
  test("PointOnCircle should return the correct point on the circle for a given angle"):
    val center = Vector2D(0, 0)
    val radius = 5.0
    val angle = math.Pi / 4

    val expectedPoint = Vector2D(
      center.x + radius * math.cos(angle),
      center.y + radius * math.sin(angle)
    )

    val result = PointOnCircle(center, radius, angle)

    result shouldBe expectedPoint
