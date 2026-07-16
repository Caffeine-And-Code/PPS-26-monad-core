package monad_core.graphics.resources

import monad_core.engine.errors.EngineError
import monad_core.graphics.ImageResourceNotFound
import scalafx.scene.image.Image as ScalaFxImage

object ImageLoader {
  private def getPath(image: Image)
                     (using imageConfig: ImageConfigRecord): String =
    imageConfig.imageBasePath + image.fileName

  def getScalaFxImage(image: Image)
                     (using imageConfig: ImageConfigRecord): Either[EngineError, ScalaFxImage] =
    val stream = getClass.getResourceAsStream(getPath(image))

    if stream == null then
      Left(ImageResourceNotFound(image))
    else
      Right(
        ScalaFxImage(
          stream,
          image.width,
          image.height,
          image.preserveRatio,
          image.preserveRatio
        )
      )
}
