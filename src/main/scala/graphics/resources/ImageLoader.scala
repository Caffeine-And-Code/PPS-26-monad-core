package graphics.resources

import engine.errors.EngineError
import graphics.ImageResourceNotFound
import graphics.resources.Image
import scalafx.scene.image.Image as ScalaFxImage

object ImageLoader {
  // TODO: this should be given by a using test this file to the fullest potential
  private val basePath = "/"

  private def getPath(image: Image): String = basePath + image.fileName

  def getScalaFxImage(image: Image): Either[EngineError, ScalaFxImage] =
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
