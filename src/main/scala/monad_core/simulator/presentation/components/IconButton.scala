package monad_core.simulator.presentation.components

import monad_core.simulator.CannotBuildButton
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.resources.{Image, ImageConfigRecord, ImageLoader}
import scalafx.beans.property.BooleanProperty
import scalafx.beans.value.ObservableValue
import scalafx.scene.control.Button
import scalafx.scene.image.{ImageView, Image as FxImage}

case class IconButtonBaseProps(
    imageConfig: ImageConfigRecord,
    additionalStyle: String = "",
    onClick: Boolean => Unit = (_: Boolean) => (),
    isDisabled: ObservableValue[Boolean, java.lang.Boolean] = BooleanProperty(false)
)

object IconButton {

  private val defaultStyle: String = "-fx-background-color: transparent; -fx-cursor: hand;"

  private case class SimpleToggleViewModel(onClick: Boolean => Unit):
    val isActive: BooleanProperty = BooleanProperty(false)

  private case class IconToggleViewModel(onClick: Boolean => Unit):
    val isActive: BooleanProperty = BooleanProperty(false)

  extension (viewModel: SimpleToggleViewModel)

    private def onToggle(): Unit =
      val newIsActive = !viewModel.isActive.value
      viewModel.isActive.value = newIsActive
      viewModel.onClick(newIsActive)

  extension (viewModel: IconToggleViewModel)

    private def onToggle(iconView: ImageView, defaultImage: FxImage, activeImage: FxImage): Unit =
      val newIsActive = !viewModel.isActive.value
      iconView.image = if newIsActive then activeImage else defaultImage
      viewModel.isActive.value = newIsActive
      viewModel.onClick(newIsActive)

  def build(
      image: Image,
      props: IconButtonBaseProps
  ): Either[BaseError, Button] =
    for loadedImage <- ImageLoader
        .getScalaFxImage(image, props.imageConfig)
        .left
        .map(error => CannotBuildButton(error, IconButton.toString))
    yield
      val viewModel = SimpleToggleViewModel(props.onClick)

      new Button() {
        graphic = ImageView(loadedImage)
        style = defaultStyle + props.additionalStyle
        onAction = _ => viewModel.onToggle()
        disable <== props.isDisabled
      }

  def buildToggle(
      defaultImage: Image,
      activeImage: Image,
      props: IconButtonBaseProps,
      activeProperty: BooleanProperty = BooleanProperty(false)
  ): Either[BaseError, Button] =
    for
      loadedDefault <- ImageLoader
        .getScalaFxImage(defaultImage, props.imageConfig)
        .left
        .map(error => CannotBuildButton(error, IconButton.toString))
      loadedActive <- ImageLoader
        .getScalaFxImage(activeImage, props.imageConfig)
        .left
        .map(error => CannotBuildButton(error, IconButton.toString))
    yield
      val viewModel = IconToggleViewModel(props.onClick)
      val iconView  = ImageView(loadedDefault)

      activeProperty.onChange { (_, _, isActive) =>
        iconView.image = if isActive then loadedActive else loadedDefault
      }

      new Button() {
        graphic = iconView
        style = defaultStyle + props.additionalStyle
        disable <== props.isDisabled
        onAction = _ => viewModel.onToggle(iconView, loadedDefault, loadedActive)
      }

}
