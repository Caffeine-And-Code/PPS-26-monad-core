package graphics.resources

import graphics.ImageResourceNotFound
import graphics.resources.Image.PlayIcon
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.scene.image.Image as ScalaFxImage

class ImageLoaderTest extends AnyFunSuite with Inside with Matchers with MockFactory:

  test("An invalid Image load should respond with an error"):
    val image = mock[Image]

    val result = ImageLoader.getScalaFxImage(image)

    inside(result):
      case Left(error) =>
        error should be(ImageResourceNotFound(image))

  test("A valid Image can be loaded"):
    val image = PlayIcon()

    val result = ImageLoader.getScalaFxImage(image)

    inside(result):
      case Right(loadedImage) =>
        loadedImage shouldBe a[ScalaFxImage]
