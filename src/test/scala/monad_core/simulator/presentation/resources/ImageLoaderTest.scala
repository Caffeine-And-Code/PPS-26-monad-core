package monad_core.simulator.presentation.resources

import helpers.mocks.{MockImage, MockImageConfig}
import monad_core.engine.model.EngineError
import monad_core.simulator.ImageResourceNotFound
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.resources.{Image, ImageConfigRecord, ImageLoader}
import monad_core.simulator.presentation.resources.Image.PerformanceIcon
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.scene.image.Image as ScalaFxImage

class ImageLoaderTest extends AnyFunSuite with Inside with Matchers with MockFactory:
  test("An invalid Image load should respond with an error"):
    val imageConfig: ImageConfigRecord = mock[ImageConfigRecord]
    val image                          = mock[Image]

    val result = ImageLoader.getScalaFxImage(image, imageConfig)

    inside(result):
      case Left(error) =>
        error should be(ImageResourceNotFound(image))

  test("A valid Image can be loaded"):
    val imageConfig: ImageConfigRecord = MockImageConfig()
    val image: Image                   = MockImage()

    val result: Either[BaseError, ScalaFxImage] = ImageLoader.getScalaFxImage(image, imageConfig)

    inside(result):
      case Right(loadedImage) =>
        loadedImage shouldBe a[ScalaFxImage]
