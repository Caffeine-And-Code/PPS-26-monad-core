package monad_core.simulator.presentation.stages.support

import monad_core.simulator.InvalidSizeValue
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SizeTest extends AnyFunSuite with Inside with Matchers with MockFactory:

  test("a squared Size can be created"):
    val expectedEdgeLength = 32.0

    val sizeResult = Size.square(expectedEdgeLength)

    inside(sizeResult):
      case Right(size) =>
        size.width should be(expectedEdgeLength)
        size.height should be(expectedEdgeLength)

  test("a squared Size returns error when passing an invalid value"):
    val expectedEdgeLength = -16.0

    val sizeResult = Size.square(expectedEdgeLength)

    inside(sizeResult):
      case Left(error) =>
        error shouldBe a[InvalidSizeValue]

  test("a rectangle Size returns error when passing an invalid width"):
    val invalidWidth = -32.0
    val height = 16.0

    val sizeResult = Size.rectangle(invalidWidth, height)

    inside(sizeResult):
      case Left(error) =>
        error shouldBe a[InvalidSizeValue]

  test("a rectangle Size returns error when passing an invalid height"):
    val invalidWidth = 32.0
    val height = -16.0

    val sizeResult = Size.rectangle(invalidWidth, height)

    inside(sizeResult):
      case Left(error) =>
        error shouldBe a[InvalidSizeValue]

  test("a rectangle Size can be created"):
    val expectedWidth = 32.0
    val expectedHeight = 16.0

    val sizeResult = Size.rectangle(expectedWidth, expectedHeight)

    inside(sizeResult):
      case Right(size) =>
        size.width should be(expectedWidth)
        size.height should be(expectedHeight)

