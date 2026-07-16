package graphics.resources

import engine.errors.EngineError
import graphics.ImageResourceNotFound
import graphics.resources.Image
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
