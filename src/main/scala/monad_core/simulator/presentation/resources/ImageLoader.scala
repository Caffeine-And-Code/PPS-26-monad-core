package monad_core.simulator.presentation.resources

import monad_core.simulator.ImageResourceNotFound
import monad_core.simulator.errors.BaseError
import scalafx.scene.image.Image as ScalaFxImage

import scala.util.Using

object ImageLoader:

  private def getPath(image: Image, imageConfig: ImageConfigRecord): String =
    imageConfig.imageBasePath + image.fileName

  def getScalaFxImage(
      image: Image,
      imageConfig: ImageConfigRecord
  ): Either[BaseError, ScalaFxImage] =
    val stream = getClass.getResourceAsStream(getPath(image, imageConfig))

    if stream == null then Left(ImageResourceNotFound(image))
    else
      val fxImage = Using.resource(stream) { s =>
        ScalaFxImage(
          s,
          image.width,
          image.height,
          image.preserveRatio,
          image.preserveRatio
        )
      }

      Right(fxImage)
