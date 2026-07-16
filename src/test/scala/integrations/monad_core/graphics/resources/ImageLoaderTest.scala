package integrations.monad_core.graphics.resources

import monad_core.graphics.resources.Image.{PauseIcon, PlayIcon, StopIcon}
import monad_core.graphics.resources.{BaseImageConfig, ImageLoader}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table
import scalafx.scene.image.Image as ScalaFxImage

class ImageLoaderTest extends AnyFunSuite with Inside with Matchers with MockFactory:

  test("each Image can be loaded by the loader"):
    val imageConfig : BaseImageConfig = BaseImageConfig()
    
    val cases = Table(
      "image",
      PlayIcon(),
      StopIcon(),
      PauseIcon()
    )

    forAll(cases): image =>
      val result = ImageLoader.getScalaFxImage(image, imageConfig)

      inside(result):
        case Right(loadedImage) => loadedImage shouldBe a[ScalaFxImage]
