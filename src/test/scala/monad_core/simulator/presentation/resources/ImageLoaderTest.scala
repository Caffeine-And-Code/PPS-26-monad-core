package monad_core.simulator.presentation.resources

import monad_core.engine.errors.EngineError
import monad_core.simulator.ImageResourceNotFound
import monad_core.simulator.presentation.resources.{Image, ImageConfigRecord, ImageLoader}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.scene.image.Image as ScalaFxImage

class ImageLoaderTest extends AnyFunSuite with Inside with Matchers with MockFactory:
  private case class MockedImageConfig() extends ImageConfigRecord("/")

  private case class MockedImage() extends Image("play.png", 0, 0)

  test("An invalid Image load should respond with an error"):
    given imageConfig: ImageConfigRecord = mock[ImageConfigRecord]

    val image = mock[Image]

    val result = ImageLoader.getScalaFxImage(image)

    inside(result):
      case Left(error) =>
        error should be(ImageResourceNotFound(image))

  test("A valid Image can be loaded"):
    given imageConfig: ImageConfigRecord = MockedImageConfig()

    val image: Image = MockedImage()

    val result: Either[EngineError, ScalaFxImage] = ImageLoader.getScalaFxImage(image)

    inside(result):
      case Right(loadedImage) =>
        loadedImage shouldBe a[ScalaFxImage]
