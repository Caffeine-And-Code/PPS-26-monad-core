package monad_core.graphics.components

import monad_core.engine.errors.EngineError
import monad_core.graphics.CannotBuildButton
import monad_core.graphics.resources.{Image, ImageConfigRecord, ImageLoader}
import scalafx.scene.control.Button
import scalafx.scene.image.ImageView

object IconButton {
  private val defaultStyle: String = "-fx-background-color: transparent; -fx-cursor: hand;"

  def build(
             image: Image,
             additionalStyle: String = "",
             onClick: () => Unit = () => (),
             isDisabled: Boolean = false,
           )
           (using imageConfig: ImageConfigRecord): Either[EngineError, Button] = {

    val loadedImageEither = ImageLoader.getScalaFxImage(image)

    loadedImageEither match
      case Left(error) => Left(CannotBuildButton(error, IconButton.toString))

      case Right(loadedImage) =>
        Right(
          new Button() {
            graphic = ImageView(loadedImage)
            style = defaultStyle + additionalStyle
            onAction = _ => onClick()
            disable = isDisabled
          }
        )
  }

  def buildToggle(
                   defaultImage: Image,
                   activeImage: Image,
                   additionalStyle: String = "",
                   onClick: Boolean => Unit = (_: Boolean) => (),
                   isDisabled: Boolean = false
                 )
                 (using imageConfig: ImageConfigRecord): Either[EngineError, Button] =

    val defaultFxImage = ImageLoader.getScalaFxImage(defaultImage)
    val activeFxImage = ImageLoader.getScalaFxImage(activeImage)

    (activeFxImage, defaultFxImage) match
      case (Right(loadedActive), Right(loadedDefault)) =>
        val iconView = ImageView(loadedDefault)

        Right(
          new Button() {
            graphic = iconView
            style = "-fx-background-color: transparent; -fx-cursor: hand;" + additionalStyle
            disable = isDisabled

            var isDefaultActive = true

            onAction = _ => {
              if isDefaultActive then
                iconView.image = loadedActive
                isDefaultActive = false
              else
                iconView.image = loadedDefault
                isDefaultActive = true

              onClick(isDefaultActive)
            }
          }
        )

      case (Left(activeError), _) => Left(CannotBuildButton(activeError, IconButton.toString))
      case (_, Left(defaultError)) => Left(CannotBuildButton(defaultError, IconButton.toString))
}
