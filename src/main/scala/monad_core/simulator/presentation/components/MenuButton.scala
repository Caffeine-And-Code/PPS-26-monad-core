package monad_core.simulator.presentation.components

import monad_core.simulator.CannotBuildButton
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.resources.{Image, ImageConfigRecord}
import scalafx.Includes.jfxScene2sfx
import scalafx.beans.property.BooleanProperty
import scalafx.beans.value.ObservableValue
import scalafx.geometry.Side
import scalafx.scene.Node
import scalafx.scene.control.{Button, CheckMenuItem, ContextMenu, MenuItem}

/**
 * Action entry displayed by a menu button.
 *
 * @param label
 *   text displayed by the menu item
 * @param onSelect
 *   callback invoked when the item is selected
 * @param isDisabled
 *   observable value bound to the menu item's disabled state
 */
final case class MenuButtonItem(
    label: String,
    onSelect: () => Unit,
    isDisabled: ObservableValue[Boolean, java.lang.Boolean] = BooleanProperty(false)
)

/**
 * Checkable entry displayed by a menu button.
 *
 * @param label
 *   text displayed by the menu item
 * @param isSelected
 *   initial checked state
 * @param onToggle
 *   callback receiving the checked state after an action
 * @param isDisabled
 *   observable value bound to the menu item's disabled state
 */
final case class CheckMenuButtonItem(
    label: String,
    isSelected: Boolean,
    onToggle: Boolean => Unit,
    isDisabled: ObservableValue[Boolean, java.lang.Boolean] = BooleanProperty(false)
)

/** Entry supported by a menu button. */
type MenuButtonEntry = MenuButtonItem | CheckMenuButtonItem

/**
 * Configuration of an icon-backed context-menu button.
 *
 * @param imageConfig
 *   configuration used to load button images
 * @param defaultImage
 *   image displayed while the menu is closed
 * @param items
 *   action and checkable entries shown in the context menu
 * @param activeImage
 *   optional image displayed while the menu is open
 * @param side
 *   side of the button on which the context menu opens
 * @param isDisabled
 *   observable value bound to the underlying button's disabled state
 */
final case class MenuButtonProps(
    imageConfig: ImageConfigRecord,
    defaultImage: Image,
    items: Seq[MenuButtonEntry],
    activeImage: Option[Image] = None,
    side: Side = Side.Bottom,
    isDisabled: ObservableValue[Boolean, java.lang.Boolean] = BooleanProperty(false)
)

/** Builds icon buttons that toggle an attached ScalaFX context menu. */
object MenuButton:

  extension (item: MenuButtonEntry)

    /**
     * Converts a declarative entry into its ScalaFX menu item.
     *
     * Disabled state remains bound to the entry observable. Checkable entries preserve their initial selection and
     * report the new selection after each action.
     *
     * @return
     *   the configured ScalaFX menu item
     */
    def toMenuItem: MenuItem =
      item match
        case action: MenuButtonItem =>
          new MenuItem(action.label):
            disable <== action.isDisabled
            onAction = _ => action.onSelect()
        case toggle: CheckMenuButtonItem =>
          new CheckMenuItem(toggle.label):
            selected = toggle.isSelected
            disable <== toggle.isDisabled
            onAction = _ => toggle.onToggle(selected.value)

  private[presentation] val StylesheetPath: String =
    getClass.getResource("/stylesheets/menu-button.css").toExternalForm

  /** Applies the shared visual style used by icon buttons in the simulator menu bar. */
  private[presentation] def styleIconButton(button: Button): Button =
    button.styleClass += "menu-icon-button"
    button.stylesheets += StylesheetPath
    button

  private case class MenuButtonViewModel(side: Side):
    val isOpen: BooleanProperty = BooleanProperty(false)

  extension (viewModel: MenuButtonViewModel)

    private def onMenuShown(): Unit = viewModel.isOpen.value = true

    private def onMenuHidden(): Unit = viewModel.isOpen.value = false

    private def toggle(contextMenu: ContextMenu, anchor: Node): Unit =
      if contextMenu.showing.value then contextMenu.hide()
      else contextMenu.show(anchor, viewModel.side, 0, 0)

  /**
   * Builds the icon button and its context menu.
   *
   * Clicking the button alternates between showing and hiding the menu. When an active image is provided, the icon
   * follows the menu's visible state. Image-loading failures are wrapped as `CannotBuildButton`.
   *
   * @param props
   *   images, entries, placement and disabled-state configuration
   * @return
   *   the configured button node, or a wrapped image-loading error
   */
  def build(props: MenuButtonProps): Either[BaseError, Node] =
    val viewModel = MenuButtonViewModel(props.side)

    val contextMenu = new ContextMenu {
      styleClass += "app-context-menu"
      onShowing = _ =>
        if !scene.value.stylesheets.contains(StylesheetPath) then
          scene.value.stylesheets.add(StylesheetPath)
      onShown = _ => viewModel.onMenuShown()
      onHidden = _ => viewModel.onMenuHidden()
    }

    contextMenu.items ++= props.items.map(_.toMenuItem)

    val buttonEither = props.activeImage match
      case Some(activeImg) =>
        IconButton.buildToggle(
          defaultImage = props.defaultImage,
          activeImage = activeImg,
          props = IconButtonBaseProps(
            props.imageConfig,
            isDisabled = props.isDisabled
          ),
          activeProperty = viewModel.isOpen
        )
      case None =>
        IconButton.build(
          props.defaultImage,
          IconButtonBaseProps(
            props.imageConfig,
            isDisabled = props.isDisabled
          )
        )

    buttonEither
      .map { btn =>
        styleIconButton(btn)
        btn.onMouseClicked = _ => viewModel.toggle(contextMenu, btn)
        btn
      }
      .left
      .map(error => CannotBuildButton(error, MenuButton.toString))
