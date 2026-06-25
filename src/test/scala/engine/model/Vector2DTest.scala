package engine.model

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

