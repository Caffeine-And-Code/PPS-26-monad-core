package monad_core.simulator.presentation.panels

import javafx.scene.input.{MouseButton, MouseEvent}
import monad_core.engine.errors.EngineError
import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.model.{**, Entity, Locatable, Surface}
import monad_core.engine.public_api.Painter
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.{SaveEntityCommand, SaveSurfaceCommand, World}
import monad_core.simulator.presentation.components.MenuButton.toMenuItem
import monad_core.simulator.presentation.components.forms.{
  SaveEntityFormDialog,
  SaveEntityFormDialogProps,
  SaveSurfaceFormDialog,
  SaveSurfaceFormDialogProps
}
import monad_core.simulator.presentation.components.{
  Error,
  MenuButtonItem,
  NotificationManager,
  ResizableCanvas
}
import monad_core.simulator.presentation.painters.Drawer
import monad_core.simulator.presentation.panels.MouseHitDetector.checkMouseHit
import monad_core.simulator.presentation.panels.support.PanelStyles
import monad_core.simulator.presentation.panels.traits.SceneRendererPanelBuilder
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

  def build()(using
      gameEngineRuntime: GameEngineRuntime,
      world: World
  ): Either[EngineError, VBox] =
    given Painter = Drawer

    val canvas                       = ResizableCanvas()
    val menusAnchor                  = Some(canvas)
    val onError: EngineError => Unit = error => NotificationManager.show(error.message, Error)

    EntityContextMenu.attachTo(
      canvas = canvas,
      findEntityAt = (x, y) =>
        val clickableElements: List[Locatable] =
          List.from(world.getAllEntities).appendedAll(world.getAllSurfaces)

        clickableElements.find(_.checkMouseHit(x, y))
      ,
      buildMenuItems = {
        case entity: Entity =>
          Seq(
            MenuButtonItem(
              s"Edit ${entity.id} Entity",
              () =>
                SaveEntityFormDialog.show(
                  props = SaveEntityFormDialogProps(
                    title = "Entity Settings",
                    anchorNode = menusAnchor,
                    onSubmit = entity => world.updateEntity(SaveEntityCommand(entity)),
                    teams = world.getAllTeams,
                    onError = onError,
                    entityToUpdate = Some(entity)
                  )
                )
            ),
            MenuButtonItem(s"Remove ${entity.id} Entity", () => world.removeEntity(entity.id))
          )
        case surface: Surface =>
          Seq(
            MenuButtonItem(
              s"Edit ${surface.id} Surface",
              () =>
                SaveSurfaceFormDialog.show(
                  props = SaveSurfaceFormDialogProps(
                    title = "Surface Settings",
                    anchorNode = menusAnchor,
                    onSubmit = surface => world.updateSurface(SaveSurfaceCommand(surface)),
                    onError = onError,
                    surfaceToUpdate = Some(surface)
                  )
                )
            ),
            MenuButtonItem(s"Remove ${surface.id} Surface", () => world.removeSurface(surface.id))
          )
      }
    )

    val onFrame: World => Unit = _ => Drawer.flush(canvas.graphicsContext2D)

    gameEngineRuntime.attach(onFrame)
    val gameEngineError = gameEngineRuntime.getError
    if gameEngineError.isDefined then Left(gameEngineError.get)
    else
      gameEngineRuntime.reset(world)

      val container = new VBox:
        children = Seq(canvas)
        style = PanelStyles.base

      VBox.setVgrow(canvas, Priority.Always)

      Right(container)
