package monad_core.simulator.presentation.panels

import javafx.scene.control.MenuItem
import javafx.scene.input.{MouseButton, MouseEvent}
import monad_core.engine.errors.EngineError
import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.model.{**, Entity, Locatable, Surface}
import monad_core.engine.public_api.Painter
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.{SaveEntityCommand, World}
import monad_core.simulator.presentation.components.MenuButton.toMenuItem
import monad_core.simulator.presentation.components.forms.{SaveEntityFormDialog, SaveEntityFormDialogProps}
import monad_core.simulator.presentation.components.{MenuButtonItem, ResizableCanvas}
import monad_core.simulator.presentation.painters.Drawer
import monad_core.simulator.presentation.panels.MouseHitDetector.checkMouseHit
import monad_core.simulator.presentation.panels.support.PanelStyles
import monad_core.simulator.presentation.panels.traits.SceneRendererPanelBuilder
import scalafx.Includes.{jfxScene2sfx, jfxWindow2sfx}
import scalafx.scene.control.ContextMenu
import scalafx.scene.layout.{Priority, VBox}

private[panels] object MouseHitDetector:
  extension (clickable: Locatable)
    def checkMouseHit(mouseClickX: Double, mouseClickY: Double): Boolean =
      val elementPositionX = clickable.position.x
      val elementPositionY = clickable.position.y

      clickable.shape match
        case Circle(radius) =>
          val distanceX = mouseClickX - elementPositionX
          val distanceY = mouseClickY - elementPositionY
          (distanceX ** 2 + distanceY ** 2) <= (radius ** 2)

        case Rectangle(width, height) =>
          val halfW = width / 2
          val halfH = height / 2
          mouseClickX >= (elementPositionX - halfW) && mouseClickX <= (elementPositionX + halfW) &&
            mouseClickY >= (elementPositionY - halfH) && mouseClickY <= (elementPositionY + halfH)

private[panels] object EntityContextMenu:

  def attachTo(
                canvas: javafx.scene.canvas.Canvas,
                findEntityAt: (Double, Double) => Option[Locatable],
                buildMenuItems: Locatable => Seq[MenuButtonItem]
              ): Unit =

    val contextMenu = new ContextMenu:
      styleClass += "app-context-menu"

    canvas.setOnMouseClicked((e: MouseEvent) =>
      contextMenu.hide()

      if e.getButton == MouseButton.SECONDARY then
        val localX = e.getX
        val localY = e.getY

        findEntityAt(localX, localY) match
          case Some(entity) =>
            contextMenu.items.clear()
            contextMenu.items ++= buildMenuItems(entity).map(_.toMenuItem)

            contextMenu.show(canvas, e.getScreenX, e.getScreenY)

          case None =>
            contextMenu.hide()
    )

object SceneRendererPanel extends SceneRendererPanelBuilder:
  def build()
           (
             using gameEngineRuntime: GameEngineRuntime,
             world: World
           )
  : Either[EngineError, VBox] =
    val canvas = ResizableCanvas()
    val drawer = Drawer

    given Painter = drawer

    EntityContextMenu.attachTo(
      canvas = canvas,
      findEntityAt = (x, y) =>
        val clickableElements: List[Locatable] =
          List.from(world.getAllEntities).appendedAll(world.getAllSurfaces)

        clickableElements.find(_.checkMouseHit(x, y)),
      buildMenuItems = {
        case entity: Entity => Seq(
          MenuButtonItem(s"Edit ${entity.id} Entity", () => SaveEntityFormDialog.show(
            props = SaveEntityFormDialogProps(
              title = "Entity Settings",
              owner = Some(canvas.scene.value.window.value),
              onSubmit = entity => world.updateEntity(SaveEntityCommand(entity)),
              teams = world.getAllTeams,
              onError = error => println(error.message),
              entityToUpdate = Some(entity)
            )
          )),
          MenuButtonItem(s"Remove ${entity.id} Entity", () => world.removeEntity(entity.id))
        )
        case surface: Surface => Seq(
          MenuButtonItem(s"Edit ${surface.id} Surface", () => println("edit surface")),
          MenuButtonItem(s"Remove ${surface.id} Surface", () => world.removeSurface(surface.id))
        )
      }
    )

    val onFrame: World => Unit = _ => drawer.flush(canvas.graphicsContext2D)

    gameEngineRuntime.attach(onFrame)
    gameEngineRuntime.reset(world)

    val container = new VBox:
      children = Seq(canvas)
      style = PanelStyles.base

    VBox.setVgrow(canvas, Priority.Always)

    Right(container)