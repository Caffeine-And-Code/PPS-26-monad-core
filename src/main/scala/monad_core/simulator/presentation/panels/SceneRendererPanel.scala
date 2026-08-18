package monad_core.simulator.presentation.panels

import javafx.scene.input.{MouseButton, MouseEvent}
import monad_core.engine.model.{Entity, Shape2D, Surface, Vector2D}
import monad_core.engine.simulator.Painter
import monad_core.simulator.application.engine.world.{SaveEntityCommand, SaveSurfaceCommand, World}
import monad_core.simulator.application.engine.{GameEngineRuntime, ShapeArchitect}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.MenuButton.toMenuItem
import monad_core.simulator.presentation.components.forms.{
  SaveEntityFormDialog,
  SaveEntityFormDialogProps,
  SaveSurfaceFormDialog,
  SaveSurfaceFormDialogProps
}
import monad_core.simulator.presentation.components.{MenuButtonItem, ResizableCanvas}
import monad_core.simulator.presentation.painters.ShapePainter
import monad_core.simulator.presentation.panels.MouseHitDetector.checkMouseHit
import monad_core.simulator.presentation.panels.support.FormUtilities.{
  displayError,
  onActionMakeSnapshot
}
import monad_core.simulator.presentation.panels.support.PanelStyles
import monad_core.simulator.presentation.panels.traits.SceneRendererPanelBuilder
import scalafx.scene.control.ContextMenu
import scalafx.scene.layout.{Priority, VBox}

type Clickable = Surface | Entity

private[panels] object MouseHitDetector:

  extension (clickable: Clickable)

    private def checkGenericHit(
        position: Vector2D,
        clickPosition: (Double, Double),
        shape: Shape2D
    ): Boolean =
      val elementPositionX = position._1
      val elementPositionY = position._2
      val mouseClickX      = clickPosition._1
      val mouseClickY      = clickPosition._2

      shape match
        case Shape2D.Circle(radius) =>
          val distanceX = mouseClickX - elementPositionX
          val distanceY = mouseClickY - elementPositionY

          (distanceX * distanceX + distanceY * distanceY) <= (radius * radius)

        case Shape2D.Rectangle(height, width) =>
          val halfW = width / 2
          val halfH = height / 2

          mouseClickX >= (elementPositionX - halfW) && mouseClickX <= (elementPositionX + halfW) &&
          mouseClickY >= (elementPositionY - halfH) && mouseClickY <= (elementPositionY + halfH)

    def checkMouseHit(mouseClick: (Double, Double)): Boolean =
      clickable match
        case surface: Surface =>
          checkGenericHit(surface.position, mouseClick, surface.shape)
        case entity: Entity => checkGenericHit(entity.position, mouseClick, entity.shape)

private[panels] object EntityContextMenu:

  def attachTo(
      canvas: javafx.scene.canvas.Canvas,
      findEntityAt: (Double, Double) => Option[Clickable],
      buildMenuItems: Clickable => Seq[MenuButtonItem]
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
      world: World,
      architect: ShapeArchitect,
      painter: Painter
  ): Either[BaseError, VBox] =

    val canvas      = ResizableCanvas()
    val menusAnchor = Some(canvas)

    def resizeWorld(): Unit =
      val width  = canvas.width.value
      val height = canvas.height.value

      if width > 0 && height > 0 then
        gameEngineRuntime.resize(width, height)

    canvas.width.onChange {
      resizeWorld()
    }

    canvas.height.onChange {
      resizeWorld()
    }

    resizeWorld()

    val findEntitiesAt: (Double, Double) => Option[Clickable] = (x, y) =>
      val entities: List[Clickable]          = world.getAllEntities
      val surfaces: List[Clickable]          = world.getAllSurfaces
      val clickableElements: List[Clickable] = entities ++ surfaces
      clickableElements.find(_.checkMouseHit((x, y)))

    EntityContextMenu.attachTo(
      canvas = canvas,
      findEntityAt = findEntitiesAt,
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
                    onSubmit =
                      entity => onActionMakeSnapshot(SaveEntityCommand(entity), world.updateEntity),
                    teams = world.getAllTeams,
                    onError = displayError,
                    entityToUpdate = Some(entity)
                  )
                )
            ),
            MenuButtonItem(
              s"Remove ${entity.id} Entity",
              () => onActionMakeSnapshot(entity.id, id => world.removeEntity(id.value))
            )
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
                    onSubmit = surface =>
                      onActionMakeSnapshot(SaveSurfaceCommand(surface), world.updateSurface),
                    onError = displayError,
                    surfaceToUpdate = Some(surface)
                  )
                )
            ),
            MenuButtonItem(
              s"Remove ${surface.id} Surface",
              () => onActionMakeSnapshot(surface.id, id => world.removeSurface(id.value))
            )
          )
      }
    )

    val onFrame: World => Unit = _ => ShapePainter.paint(canvas.graphicsContext2D)

    gameEngineRuntime.attach(onFrame)
    val gameEngineError = gameEngineRuntime.getError

    if gameEngineError.isDefined then Left(gameEngineError.get)
    else
      gameEngineRuntime.resetToSnapshot()

      val container = new VBox:
        children = Seq(canvas)
        style = PanelStyles.base

      VBox.setVgrow(canvas, Priority.Always)

      Right(container)
