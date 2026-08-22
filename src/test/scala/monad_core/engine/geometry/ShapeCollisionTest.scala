package monad_core.engine.geometry

import monad_core.engine.geometry.{Collision, Placed, ShapeCollision}
import monad_core.engine.model.*
import org.scalatest.EitherValues.*
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.*

class ShapeCollisionTest extends AnyFunSuite with Matchers:

  private val Epsilon = 1e-9

  test("circle collides with another circle"):
    val cases = Table(
      (
        "firstCircle",
        "secondCircle",
        "collision"
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.circle(2).value),
        Placed(Vector2D(3, 0), Shape2D.circle(2).value),
        Collision(Vector2D(1, 0), 1, Vector2D(1.5, 0))
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.circle(2).value),
        Placed(Vector2D(0, 3), Shape2D.circle(2).value),
        Collision(Vector2D(0, 1), 1, Vector2D(0, 1.5))
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.circle(2).value),
        Placed(Vector2D(-3, 0), Shape2D.circle(2).value),
        Collision(Vector2D(-1, 0), 1, Vector2D(-1.5, 0))
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.circle(2).value),
        Placed(Vector2D(0, -3), Shape2D.circle(2).value),
        Collision(Vector2D(0, -1), 1, Vector2D(0, -1.5))
      )
    )

    forAll(cases): (firstCircle, secondCircle, collision) =>

      val result = ShapeCollision.circleCollidesWithCircle.checkCollision(firstCircle, secondCircle)

      result shouldBe Some(collision)

  test("circle not collides with another circle"):
    val firstCircle  = Placed(Vector2D(3, 3), Shape2D.circle(9).value)
    val secondCircle = Placed(Vector2D(15, 3), Shape2D.circle(2).value)

    val result = ShapeCollision.circleCollidesWithCircle.checkCollision(firstCircle, secondCircle)

    result shouldBe None

  test("circles touching at their boundaries collide with zero penetration"):
    val firstCircle  = Placed(Vector2D(0, 0), Shape2D.circle(2).value)
    val secondCircle = Placed(Vector2D(4, 0), Shape2D.circle(2).value)

    val result = ShapeCollision.circleCollidesWithCircle.checkCollision(firstCircle, secondCircle)

    result shouldBe Some(Collision(Vector2D(1, 0), 0, Vector2D(2, 0)))

  test("concentric circles returns a collision"):
    val first  = Placed(Vector2D(0, 0), Shape2D.circle(2).value)
    val second = Placed(Vector2D(0, 0), Shape2D.circle(1).value)

    val collision = ShapeCollision.circleCollidesWithCircle.checkCollision(first, second).value

    collision.normalVector shouldBe Vector2D(1, 0)
    collision.penetrationDepth shouldBe 3.0

  test("circles separated by exactly epsilon use the fallback collision normal"):
    val first  = Placed(Vector2D(0, 0), Shape2D.circle(2).value)
    val second = Placed(Vector2D(0, Epsilon), Shape2D.circle(1).value)

    val collision = ShapeCollision.circleCollidesWithCircle.checkCollision(first, second).value

    collision.normalVector shouldBe Vector2D(1, 0)

  test("rectangle collides with another rectangle"):
    val cases = Table(
      (
        "firstRectangle",
        "secondRectangle",
        "collision"
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.rectangle(5, 5).value),
        Placed(Vector2D(2.5, 0), Shape2D.rectangle(5, 5).value),
        Collision(Vector2D(1, 0), 2.5, Vector2D(1.25, 0))
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.rectangle(5, 5).value),
        Placed(Vector2D(0, 2.5), Shape2D.rectangle(5, 5).value),
        Collision(Vector2D(0, 1), 2.5, Vector2D(0, 1.25))
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.rectangle(5, 5).value),
        Placed(Vector2D(-2.5, 0), Shape2D.rectangle(5, 5).value),
        Collision(Vector2D(-1, 0), 2.5, Vector2D(-1.25, 0))
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.rectangle(5, 5).value),
        Placed(Vector2D(0, -2.5), Shape2D.rectangle(5, 5).value),
        Collision(Vector2D(0, -1), 2.5, Vector2D(0, -1.25))
      )
    )

    forAll(cases): (firstRectangle, secondRectangle, collision) =>

      val result = ShapeCollision.rectangleCollidesWithRectangle.checkCollision(
        firstRectangle,
        secondRectangle
      )

      result shouldBe Some(collision)

  test("rectangle not collides with another rectangle"):
    val firstRectangle  = Placed(Vector2D(0, 0), Shape2D.rectangle(5.9, 5.9).value)
    val secondRectangle = Placed(Vector2D(6, 0), Shape2D.rectangle(5.9, 5.9).value)

    val result =
      ShapeCollision.rectangleCollidesWithRectangle.checkCollision(firstRectangle, secondRectangle)

    result shouldBe None

  test("rectangles touching at their boundaries collide with zero penetration"):
    val firstRectangle  = Placed(Vector2D(0, 0), Shape2D.rectangle(4, 4).value)
    val secondRectangle = Placed(Vector2D(4, 0), Shape2D.rectangle(4, 4).value)

    val result =
      ShapeCollision.rectangleCollidesWithRectangle.checkCollision(firstRectangle, secondRectangle)

    result shouldBe Some(Collision(Vector2D(1, 0), 0, Vector2D(2, 0)))

  test("rectangles touching vertically collide with zero penetration"):
    val firstRectangle  = Placed(Vector2D(0, 0), Shape2D.rectangle(4, 4).value)
    val secondRectangle = Placed(Vector2D(0, 4), Shape2D.rectangle(4, 4).value)

    val result =
      ShapeCollision.rectangleCollidesWithRectangle.checkCollision(firstRectangle, secondRectangle)

    result shouldBe Some(Collision(Vector2D(0, 1), 0, Vector2D(0, 2)))

  test("rectangle collision returns full overlaps on the horizontal axis"):
    val firstRectangle  = Placed(Vector2D(0, 0), Shape2D.rectangle(4, 4).value)
    val secondRectangle = Placed(Vector2D(0, 0), Shape2D.rectangle(4, 4).value)

    val result =
      ShapeCollision.rectangleCollidesWithRectangle.checkCollision(firstRectangle, secondRectangle)

    result shouldBe Some(Collision(Vector2D(1, 0), 4, Vector2D(0, 0)))

  test("rotated rectangle collision should return the center of the contact region"):
    val first  = Placed(Vector2D(10.0, 10.0), Shape2D.rectangle(2.0, 4.0).value, 90.0)
    val second = Placed(Vector2D(10.0, 13.0), Shape2D.rectangle(2.0, 4.0).value, 90.0)

    val collision = ShapeCollision.rectangleCollidesWithRectangle
      .checkCollision(first, second)
      .value

    collision.normalVector.x shouldBe 0.0 +- Epsilon
    collision.normalVector.y shouldBe 1.0 +- Epsilon
    collision.penetrationDepth shouldBe 1.0 +- Epsilon
    collision.collisionPoint.x shouldBe 10.0 +- Epsilon
    collision.collisionPoint.y shouldBe 11.5 +- Epsilon

  test("rectangle contact point should remain stable across tiny rotations"):
    val first = Placed(Vector2D(0, 0), Shape2D.rectangle(2, 4).value)
    val clockwise = Placed(
      Vector2D(0, 1.9),
      Shape2D.rectangle(2, 4).value,
      -Epsilon
    )
    val counterClockwise = clockwise.copy(rotation = Epsilon)

    val firstPoint = ShapeCollision.rectangleCollidesWithRectangle
      .checkCollision(first, clockwise)
      .value
      .collisionPoint
    val secondPoint = ShapeCollision.rectangleCollidesWithRectangle
      .checkCollision(first, counterClockwise)
      .value
      .collisionPoint

    math.hypot(firstPoint.x - secondPoint.x, firstPoint.y - secondPoint.y) should be < Epsilon

  test("rectangle clipping includes vertices exactly at epsilon"):
    val first = Placed(
      Vector2D(Epsilon, 0.0),
      Shape2D.rectangle(2.0 * Epsilon, 2.0 * Epsilon).value
    )
    val second = Placed(
      Vector2D(0.0, 0.0),
      Shape2D.rectangle(2.0 * Epsilon, 2.0 * Epsilon).value
    )

    val collision = ShapeCollision.rectangleCollidesWithRectangle
      .checkCollision(first, second)
      .value

    collision.collisionPoint.x shouldBe Epsilon
    collision.collisionPoint.y shouldBe 0.0

  test("intersection center falls back to the midpoint when clipping is empty"):
    val first = Placed(
      Vector2D(-10.0, 2.0),
      Shape2D.rectangle(2.0, 2.0).value
    )
    val second = Placed(
      Vector2D(10.0, 4.0),
      Shape2D.rectangle(2.0, 2.0).value
    )

    ShapeCollision.intersectionCenter(first, second) shouldBe Vector2D(0.0, 3.0)

  test("circle collides with a rectangle"):
    val cases = Table(
      (
        "circle",
        "rectangle",
        "collision"
      ),
      (
        Placed(Vector2D(6, 0), Shape2D.circle(4).value),
        Placed(Vector2D(0, 0), Shape2D.rectangle(6, 6).value),
        Collision(Vector2D(-1, 0), 1, Vector2D(3, 0))
      ),
      (
        Placed(Vector2D(0, 6), Shape2D.circle(4).value),
        Placed(Vector2D(0, 0), Shape2D.rectangle(6, 6).value),
        Collision(Vector2D(0, -1), 1, Vector2D(0, 3))
      ),
      (
        Placed(Vector2D(-6, 0), Shape2D.circle(4).value),
        Placed(Vector2D(0, 0), Shape2D.rectangle(6, 6).value),
        Collision(Vector2D(1, 0), 1, Vector2D(-3, 0))
      ),
      (
        Placed(Vector2D(0, -6), Shape2D.circle(4).value),
        Placed(Vector2D(0, 0), Shape2D.rectangle(6, 6).value),
        Collision(Vector2D(0, 1), 1, Vector2D(0, -3))
      )
    )

    forAll(cases): (circle, rectangle, collision) =>

      val result = ShapeCollision.circleCollidesWithRectangle.checkCollision(circle, rectangle)

      result shouldBe Some(collision)

  test("circle not collides with another rectangle"):
    val circle    = Placed(Vector2D(0, 0), Shape2D.circle(2.9).value)
    val rectangle = Placed(Vector2D(6, 0), Shape2D.rectangle(6, 6).value)

    val result = ShapeCollision.circleCollidesWithRectangle.checkCollision(circle, rectangle)

    result shouldBe None

  test("circle collision with a rotated rectangle returns the world collision point"):
    val circle    = Placed(Vector2D(0, 4), Shape2D.circle(2).value)
    val rectangle = Placed(Vector2D(0, 0), Shape2D.rectangle(2, 6).value, 90)

    val collision = ShapeCollision.circleCollidesWithRectangle.checkCollision(circle, rectangle).get

    collision.normalVector.x shouldBe 0.0 +- Epsilon
    collision.normalVector.y shouldBe -1.0 +- Epsilon
    collision.penetrationDepth shouldBe 1.0 +- Epsilon
    collision.collisionPoint.x shouldBe 0.0 +- Epsilon
    collision.collisionPoint.y shouldBe 3.0 +- Epsilon

  test("a circle touching a rectangle boundary collides with zero penetration"):
    val circle    = Placed(Vector2D(5, 0), Shape2D.circle(2).value)
    val rectangle = Placed(Vector2D(0, 0), Shape2D.rectangle(6, 6).value)

    val result = ShapeCollision.circleCollidesWithRectangle.checkCollision(circle, rectangle)

    result shouldBe Some(Collision(Vector2D(-1, 0), 0, Vector2D(3, 0)))

  test("circle collides with another rectangle too if it is fully inside it"):

    val cases = Table(
      (
        "circle",
        "rectangle",
        "collision"
      ),
      (
        Placed(Vector2D(7, 0), Shape2D.circle(2).value),
        Placed(Vector2D(6, 0), Shape2D.rectangle(6, 6).value),
        Collision(Vector2D(-1, 0), 4, Vector2D(9, 0))
      ),
      (
        Placed(Vector2D(6, 1), Shape2D.circle(2).value),
        Placed(Vector2D(6, 0), Shape2D.rectangle(6, 6).value),
        Collision(Vector2D(0, -1), 4, Vector2D(6, 3))
      ),
      (
        Placed(Vector2D(5, 0), Shape2D.circle(2).value),
        Placed(Vector2D(6, 0), Shape2D.rectangle(6, 6).value),
        Collision(Vector2D(1, 0), 4, Vector2D(3, 0))
      ),
      (
        Placed(Vector2D(6, -1), Shape2D.circle(2).value),
        Placed(Vector2D(6, 0), Shape2D.rectangle(6, 6).value),
        Collision(Vector2D(0, 1), 4, Vector2D(6, -3))
      )
    )

    forAll(cases): (circle, rectangle, collision) =>

      val result = ShapeCollision.circleCollidesWithRectangle.checkCollision(circle, rectangle)

      result shouldBe Some(collision)

  test("rectangle collides with a circle"):
    val cases = Table(
      (
        "rectangle",
        "circle",
        "collision"
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.rectangle(6, 6).value),
        Placed(Vector2D(6, 0), Shape2D.circle(4).value),
        Collision(Vector2D(1, 0), 1, Vector2D(3, 0))
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.rectangle(6, 6).value),
        Placed(Vector2D(0, 6), Shape2D.circle(4).value),
        Collision(Vector2D(0, 1), 1, Vector2D(0, 3))
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.rectangle(6, 6).value),
        Placed(Vector2D(-6, 0), Shape2D.circle(4).value),
        Collision(Vector2D(-1, 0), 1, Vector2D(-3, 0))
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.rectangle(6, 6).value),
        Placed(Vector2D(0, -6), Shape2D.circle(4).value),
        Collision(Vector2D(0, -1), 1, Vector2D(0, -3))
      )
    )

    forAll(cases): (rectangle, circle, collision) =>

      val result = ShapeCollision.rectangleCollidesWithCircle.checkCollision(rectangle, circle)

      result shouldBe Some(collision)

  test("separated rotated rectangles should not collide"):
    val first  = Placed(Vector2D(0.0, 0.0), Shape2D.rectangle(2.0, 6.0).value, 45.0)
    val second = Placed(Vector2D(10.0, 10.0), Shape2D.rectangle(3.0, 4.0).value, -30.0)

    ShapeCollision.rectangleCollidesWithRectangle.checkCollision(first, second) shouldBe None

  test("rectangle collision should be symmetric"):
    val first  = Placed(Vector2D(0.0, 0.0), Shape2D.rectangle(4.0, 6.0).value, 20.0)
    val second = Placed(Vector2D(2.0, 1.0), Shape2D.rectangle(3.0, 5.0).value, -15.0)

    val direct = ShapeCollision.rectangleCollidesWithRectangle
      .checkCollision(first, second)
      .value
    val reverse = ShapeCollision.rectangleCollidesWithRectangle
      .checkCollision(second, first)
      .value

    direct.normalVector.x shouldBe -reverse.normalVector.x +- Epsilon
    direct.normalVector.y shouldBe -reverse.normalVector.y +- Epsilon
    direct.penetrationDepth shouldBe reverse.penetrationDepth +- Epsilon
    direct.collisionPoint.x shouldBe reverse.collisionPoint.x +- Epsilon
    direct.collisionPoint.y shouldBe reverse.collisionPoint.y +- Epsilon

  test("the contact region center should follow a contained rotated rectangle"):
    val outer = Placed(Vector2D(0.0, 0.0), Shape2D.rectangle(10.0, 10.0).value)
    val inner = Placed(Vector2D(1.0, 2.0), Shape2D.rectangle(2.0, 4.0).value, 30.0)

    val collision = ShapeCollision.rectangleCollidesWithRectangle
      .checkCollision(outer, inner)
      .value

    collision.collisionPoint.x shouldBe inner.center.x +- Epsilon
    collision.collisionPoint.y shouldBe inner.center.y +- Epsilon

  test("rectangle contact point should remain finite for edge contact"):
    val first = Placed(Vector2D(0.0, 0.0), Shape2D.rectangle(4.0, 4.0).value, 45.0)
    val second = Placed(
      Vector2D(4.0 * math.sqrt(2.0) - Epsilon, 0.0),
      Shape2D.rectangle(4.0, 4.0).value,
      45.0
    )

    val point = ShapeCollision.rectangleCollidesWithRectangle
      .checkCollision(first, second)
      .value
      .collisionPoint

    point.x.isFinite shouldBe true
    point.y.isFinite shouldBe true

  test("a circle inside a rotated rectangle should use world-space normal and contact point"):
    val circle = Placed(Vector2D(10.0, 11.0), Shape2D.circle(1.0).value)
    val rectangle = Placed(
      Vector2D(10.0, 10.0),
      Shape2D.rectangle(8.0, 4.0).value,
      90.0
    )

    val collision = ShapeCollision.circleCollidesWithRectangle
      .checkCollision(circle, rectangle)
      .value

    collision.normalVector.x shouldBe 0.0 +- Epsilon
    collision.normalVector.y shouldBe -1.0 +- Epsilon
    collision.penetrationDepth shouldBe 2.0 +- Epsilon
    collision.collisionPoint.x shouldBe 10.0 +- Epsilon
    collision.collisionPoint.y shouldBe 12.0 +- Epsilon

  test("a centered circle inside a rectangle should use a deterministic nearest edge"):
    val circle    = Placed(Vector2D(0.0, 0.0), Shape2D.circle(1.0).value)
    val rectangle = Placed(Vector2D(0.0, 0.0), Shape2D.rectangle(4.0, 8.0).value)

    val collision = ShapeCollision.circleCollidesWithRectangle
      .checkCollision(circle, rectangle)
      .value

    collision shouldBe Collision(Vector2D(0.0, -1.0), 3.0, Vector2D(0.0, 2.0))

  test("a circle tangent to a rectangle corner should collide with zero penetration"):
    val radius    = math.sqrt(2.0)
    val circle    = Placed(Vector2D(4.0, 4.0), Shape2D.circle(radius).value)
    val rectangle = Placed(Vector2D(0.0, 0.0), Shape2D.rectangle(6.0, 6.0).value)

    val collision = ShapeCollision.circleCollidesWithRectangle
      .checkCollision(circle, rectangle)
      .value

    collision.penetrationDepth shouldBe 0.0 +- Epsilon
    collision.normalVector.x shouldBe -1.0 / radius +- Epsilon
    collision.normalVector.y shouldBe -1.0 / radius +- Epsilon
    collision.collisionPoint shouldBe Vector2D(3.0, 3.0)

  test("a circle outside a rectangle corner should not collide"):
    val circle    = Placed(Vector2D(4.0, 4.0), Shape2D.circle(1.0).value)
    val rectangle = Placed(Vector2D(0.0, 0.0), Shape2D.rectangle(6.0, 6.0).value)

    ShapeCollision.circleCollidesWithRectangle.checkCollision(circle, rectangle) shouldBe None

  test("circle-rectangle collision should be symmetric"):
    val circle = Placed(Vector2D(2.5, 1.0), Shape2D.circle(2.0).value)
    val rectangle = Placed(
      Vector2D(0.0, 0.0),
      Shape2D.rectangle(4.0, 6.0).value,
      20.0
    )

    val direct = ShapeCollision.circleCollidesWithRectangle
      .checkCollision(circle, rectangle)
      .value
    val reverse = ShapeCollision.rectangleCollidesWithCircle
      .checkCollision(rectangle, circle)
      .value

    reverse.normalVector shouldBe direct.normalVector.flip
    reverse.penetrationDepth shouldBe direct.penetrationDepth
    reverse.collisionPoint shouldBe direct.collisionPoint

  test("shape collision dispatcher should preserve rectangle collision details"):
    val firstShape   = Shape2D.rectangle(4.0, 4.0).value
    val secondShape  = Shape2D.rectangle(4.0, 4.0).value
    val placedFirst  = Placed(Vector2D(0.0, 0.0), firstShape, 10.0)
    val placedSecond = Placed(Vector2D(2.0, 0.0), secondShape, -10.0)
    val first: Placed[Shape2D] =
      Placed(placedFirst.center, firstShape, placedFirst.rotation)
    val second: Placed[Shape2D] =
      Placed(placedSecond.center, secondShape, placedSecond.rotation)

    val expected =
      ShapeCollision.rectangleCollidesWithRectangle.checkCollision(placedFirst, placedSecond)

    ShapeCollision.shapeCollidesWithShape.checkCollision(first, second) shouldBe expected
