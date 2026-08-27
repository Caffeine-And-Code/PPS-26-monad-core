package monad_core.simulator.presentation.components

import monad_core.simulator.CannotBuildButton
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.resources.{Image, ImageConfigRecord, ImageLoader}
import scalafx.beans.property.BooleanProperty
import scalafx.beans.value.ObservableValue
import scalafx.scene.control.Button
import scalafx.scene.image.{ImageView, Image as FxImage}

/**
 * Shared configuration for image-icon buttons.
 *
 * @param imageConfig
 *   configuration used to load and size button images
 * @param additionalStyle
 *   CSS appended to the default transparent button style
 * @param onClick
 *   callback receiving the new internal active state after each click
 * @param isDisabled
 *   observable value bound to the button disabled state
 */
case class IconButtonBaseProps(
    imageConfig: ImageConfigRecord,
    additionalStyle: String = "",
    onClick: Boolean => Unit = (_: Boolean) => (),
    isDisabled: ObservableValue[Boolean, java.lang.Boolean] = BooleanProperty(false)
)

/** Builds buttons whose graphics are loaded from the application's image resources. */
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

  /**
   * Builds a button with a fixed icon and a boolean click state.
   *
   * The icon remains unchanged, while each click toggles the internal state passed to `props.onClick`.
   *
   * @param image
   *   image resource displayed by the button
   * @param props
   *   loading, styling and interaction configuration
   * @return
   *   the configured button, or a `CannotBuildButton` error if the image cannot be loaded
   */
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

  /**
   * Builds a button that alternates between default and active icons when clicked.
   *
   * Each click updates the displayed icon and passes the new internal active state to `props.onClick`. Changes to
   * `activeProperty` also update the displayed icon independently of the click state.
   *
   * @param defaultImage
   *   image displayed for the inactive state
   * @param activeImage
   *   image displayed for the active state
   * @param props
   *   loading, styling and interaction configuration
   * @param activeProperty
   *   external property whose changes select the displayed icon
   * @return
   *   the configured button, or a `CannotBuildButton` error if either image cannot be loaded
   */
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
