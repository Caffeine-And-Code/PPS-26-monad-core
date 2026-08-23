package monad_core.simulator.presentation.components

import monad_core.simulator.CannotBuildButton
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.resources.{Image, ImageConfigRecord}
import scalafx.Includes.jfxScene2sfx
import scalafx.beans.property.BooleanProperty
import scalafx.beans.value.ObservableValue
import scalafx.geometry.Side
import scalafx.scene.Node
import scalafx.scene.control.{ContextMenu, MenuItem}

final case class MenuButtonItem(
    label: String,
    onSelect: () => Unit,
    isDisabled: ObservableValue[Boolean, java.lang.Boolean] = BooleanProperty(false)
)

final case class MenuButtonProps(
    imageConfig: ImageConfigRecord,
    defaultImage: Image,
    items: Seq[MenuButtonItem],
    activeImage: Option[Image] = None,
    side: Side = Side.Bottom,
    isDisabled: ObservableValue[Boolean, java.lang.Boolean] = BooleanProperty(false)
)

object MenuButton:

  extension (item: MenuButtonItem)

    def toMenuItem: MenuItem =
      new MenuItem(item.label):
        disable <== item.isDisabled
        onAction = _ => item.onSelect()

  private val StylesheetPath: String =
    getClass.getResource("/stylesheets/menu-button.css").toExternalForm

  private case class MenuButtonViewModel(side: Side):
    val isOpen: BooleanProperty = BooleanProperty(false)

  extension (viewModel: MenuButtonViewModel)

    private def onMenuShown(): Unit = viewModel.isOpen.value = true

    private def onMenuHidden(): Unit = viewModel.isOpen.value = false

    private def toggle(contextMenu: ContextMenu, anchor: Node): Unit =
      if contextMenu.showing.value then contextMenu.hide()
      else contextMenu.show(anchor, viewModel.side, 0, 0)

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
        btn.styleClass += "menu-icon-button"
        btn.stylesheets += StylesheetPath
        btn.onMouseClicked = _ => viewModel.toggle(contextMenu, btn)
        btn
      }
      .left
      .map(error => CannotBuildButton(error, MenuButton.toString))
