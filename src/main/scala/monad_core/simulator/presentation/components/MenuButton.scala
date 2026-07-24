package monad_core.simulator.presentation.components

import monad_core.engine.errors.EngineError
import monad_core.simulator.CannotBuildButton
import monad_core.simulator.presentation.resources.{Image, ImageConfigRecord}
import scalafx.Includes.jfxScene2sfx
import scalafx.beans.property.BooleanProperty
import scalafx.scene.control.{ContextMenu, MenuItem}
import scalafx.scene.Node
import scalafx.geometry.Side

final case class MenuButtonItem(
                                 label: String,
                                 onSelect: () => Unit,
                                 isDisabled: Boolean = false
                               )

final case class MenuButtonProps(
                                  imageConfig: ImageConfigRecord,
                                  defaultImage: Image,
                                  items: Seq[MenuButtonItem],
                                  activeImage: Option[Image] = None,
                                  side: Side = Side.Bottom,
                                  isDisabled: Boolean = false
                                )

object MenuButton {

  private val StylesheetPath: String =
    getClass.getResource("/stylesheets/menu-button.css").toExternalForm

  def build(props: MenuButtonProps): Either[EngineError, Node] = {
    val isOpen = BooleanProperty(false)

    val contextMenu = new ContextMenu {
      styleClass += "app-context-menu"
      onShowing = _ =>
        if !scene.value.stylesheets.contains(StylesheetPath) then
          scene.value.stylesheets.add(StylesheetPath)
      onHidden = _ => isOpen.value = false
      onShown = _ => isOpen.value = true
    }

    contextMenu.items ++= props.items.map { item =>
      new MenuItem(item.label) {
        disable = item.isDisabled
        onAction = _ => item.onSelect()
      }
    }

    val buttonEither = props.activeImage match {
      case Some(activeImg) =>
        IconButton.buildToggle(
          defaultImage = props.defaultImage,
          activeImage = activeImg,
          props = IconButtonBaseProps(
            props.imageConfig,
            isDisabled = BooleanProperty(props.isDisabled)
          ),
          activeProperty = isOpen
        )
      case None =>
        IconButton.build(
          props.defaultImage,
          IconButtonBaseProps(
            props.imageConfig,
            isDisabled = BooleanProperty(props.isDisabled)
          )
        )
    }

    buttonEither
      .map { btn =>
        btn.styleClass += "menu-icon-button"
        btn.stylesheets += StylesheetPath

        btn.onMouseClicked = _ =>
          if contextMenu.showing.value then contextMenu.hide()
          else contextMenu.show(btn, props.side, 0, 0)
        btn
      }
      .left.map(error => CannotBuildButton(error, MenuButton.toString))
  }
}