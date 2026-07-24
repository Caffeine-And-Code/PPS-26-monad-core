package monad_core.engine.model

import monad_core.engine.model.Vector2D
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Vector2DTest extends AnyFunSuite with Matchers:

  val X = 2
  val Y = 3

  test("can create a vector 2D"):
    val vector = Vector2D(X, Y)

    vector.x shouldBe X
    vector.y shouldBe Y

  test("can sum vector 2D"):
    val vector = Vector2D(X, Y)

    val finalVector = vector + vector + vector

    finalVector.x shouldBe X * 3
    finalVector.y shouldBe Y * 3

  test("can multiply vector 2D to scalar"):
    val scalar = 3
    val vector = Vector2D(X, Y)

    val finalVector = vector * scalar

    finalVector.x shouldBe X * scalar
    finalVector.y shouldBe Y * scalar

  test("can calculate euclidean distance to another vector"):
    val vector1 = Vector2D(3, 4)
    val vector2 = Vector2D(6, 8)

    val distance = vector1 --> vector2

    distance shouldBe 5

  test("can subtract a vector to another vector"):
    val vector1 = Vector2D(6, 8)
    val vector2 = Vector2D(5, 4)

    val result = vector1 - vector2

    result shouldBe Vector2D(1, 4)

  test("can calculate the a vector"):
    val vector = Vector2D(3, 4)

    val result = vector.magnitude

    result shouldBe 5

  test("can normalize a vector"):
    val x: Double = 3
    val y: Double = 4
    val magnitude: Double = 5
    val vector = Vector2D(x, y)

    val result = vector.normalized

    result shouldBe Vector2D(x / magnitude, y / magnitude)

  test("normalized vector of a vector with magnitude 0 is Vector with x=0, y=0"):
    val vector = Vector2D(0, 0)

    val result = vector.normalized

    result shouldBe Vector2D(0, 0)

  test("can flip a vector"):
    val vector = Vector2D(1, 2)

    val result = vector.flip

    result shouldBe Vector2D(-1, -2)