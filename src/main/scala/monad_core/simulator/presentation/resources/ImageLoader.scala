package monad_core.simulator.presentation.resources

import monad_core.simulator.ImageResourceNotFound
import monad_core.simulator.errors.BaseError
import scalafx.scene.image.Image as ScalaFxImage

import scala.util.Using

/**
 * Singleton Object that provides a simple way to the Gui application to load image resources for displays purposes.
 *
 * @see [[Image]] and [[ImageConfigRecord]]
 */
object ImageLoader:

  /**
   * Interpolates the config base path with the image specific path recreating the full image path.
   *
   * @param image [[Image]] of which the path needs to be reconstructed
   * @param imageConfig [[ImageConfigRecord]] system configuration
   * @return the loadable by ScalaFx path
   */
  private def getPath(image: Image, imageConfig: ImageConfigRecord): String =
    imageConfig.imageBasePath + image.fileName

  /**
   * Tries to load the image stream from the path interpolated by [[getPath]], if found the stream is provided
   * to the ScalaFX `Image` class and the image loaded.
   *
   * @param image the [[Image]] that needs to be load
   * @param imageConfig [[ImageConfigRecord]] system configuration
   * @return `Left(ImageResourceNotFound)` if the stream cannot be found,
   *
   *         otherwise `Right(ScalaFxImage)` which is ready to use in the gui
   */
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
