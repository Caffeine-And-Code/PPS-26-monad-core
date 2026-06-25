package engine.model

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Vector2DTest extends AnyFunSuite with Matchers:

  test("can create a vector 2D"):
    val x = 2
    val y = 3

    val vector = Vector2D(x, y)

    vector.x shouldBe x
    vector.y shouldBe y

  test("can sum vector 2D"):
    val x = 2
    val y = 3

    val firstVector = Vector2D(x, y)

    val finalVector = firstVector + firstVector + firstVector

    finalVector.x shouldBe x * 3
    finalVector.y shouldBe y * 3