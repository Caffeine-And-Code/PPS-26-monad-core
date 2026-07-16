package monad_core.simulator.presentation.components

import monad_core.engine.errors.EngineError
import monad_core.simulator.CannotBuildButton
import monad_core.simulator.presentation.resources.{Image, ImageConfigRecord, ImageLoader}
import scalafx.scene.control.Button
import scalafx.scene.image.ImageView

case class IconButtonBaseProps(
                                imageConfig: ImageConfigRecord,
                                additionalStyle: String = "",
                                onClick: Boolean => Unit = (_: Boolean) => (),
                                isDisabled: Boolean = false
                              )

object IconButton {
  private val defaultStyle: String = "-fx-background-color: transparent; -fx-cursor: hand;"

  def build(
             image: Image,
             props: IconButtonBaseProps
           ): Either[EngineError, Button] =
    for
      loadedImage <- ImageLoader.getScalaFxImage(image, props.imageConfig)
        .left.map(error => CannotBuildButton(error, IconButton.toString))
    yield
      new Button() {
        graphic = ImageView(loadedImage)
        style = defaultStyle + props.additionalStyle

        var isActive = false

        onAction = _ => isActive = toggleIsActive(isActive, props.onClick)
        disable = props.isDisabled
      }

  def buildToggle(
                   defaultImage: Image,
                   activeImage: Image,
                   props: IconButtonBaseProps
                 ): Either[EngineError, Button] =
    for
      loadedDefault <- ImageLoader.getScalaFxImage(defaultImage, props.imageConfig)
        .left.map(error => CannotBuildButton(error, IconButton.toString))
      loadedActive <- ImageLoader.getScalaFxImage(activeImage, props.imageConfig)
        .left.map(error => CannotBuildButton(error, IconButton.toString))
    yield
      val iconView = ImageView(loadedDefault)

      new Button() {
        graphic = iconView
        style = defaultStyle + props.additionalStyle
        disable = props.isDisabled

        var isDefaultActive = false
        val changeIcon: Boolean => Unit =
          isActive =>
            if isActive then
              iconView.image = loadedActive
            else
              iconView.image = loadedDefault

        onAction = _ => isDefaultActive = toggleIsActive(isDefaultActive, props.onClick, changeIcon)
      }

  private def toggleIsActive(
                              currentIsActive: Boolean,
                              externalOnClick: Boolean => Unit,
                              internalOnClick: Boolean => Unit = (_: Boolean) => ()
                            ): Boolean =
    val newIsActiveValue = !currentIsActive

    if currentIsActive then
      internalOnClick(newIsActiveValue)
    else
      internalOnClick(newIsActiveValue)

    externalOnClick(newIsActiveValue)
    newIsActiveValue
}