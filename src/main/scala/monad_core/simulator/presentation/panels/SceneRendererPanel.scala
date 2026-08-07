package monad_core.simulator.presentation.panels

import javafx.scene.input.{MouseButton, MouseEvent}
import monad_core.engine.public_api.Painter
import monad_core.simulator.application.engine.world.{SaveEntityCommand, SaveSurfaceCommand, World}
import monad_core.simulator.application.engine.{GameEngineRuntime, ShapeArchitect}
import monad_core.simulator.domain.engine.MonadCoreShape.{SimulationCircle, SimulationRectangle}
import monad_core.simulator.domain.engine.{MonadCoreEntity, MonadCoreShape, MonadCoreSurface}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.MenuButton.toMenuItem
import monad_core.simulator.presentation.components.forms.{SaveEntityFormDialog, SaveEntityFormDialogProps, SaveSurfaceFormDialog, SaveSurfaceFormDialogProps}
import monad_core.simulator.presentation.components.{MenuButtonItem, ResizableCanvas}
import monad_core.simulator.presentation.painters.ShapePainter
import monad_core.simulator.presentation.panels.MouseHitDetector.checkMouseHit
import monad_core.simulator.presentation.panels.support.FormUtilities.{displayError, getTeamsSafely, onActionMakeSnapshot}
import monad_core.simulator.presentation.panels.support.PanelStyles
import monad_core.simulator.presentation.panels.traits.SceneRendererPanelBuilder
import scalafx.scene.control.ContextMenu
import scalafx.scene.layout.{Priority, VBox}

type Clickable = MonadCoreSurface | MonadCoreEntity

private[panels] object MouseHitDetector:
  extension (clickable: Clickable)
    private def checkGenericHit(position: (Double, Double), clickPosition: (Double, Double), shape: MonadCoreShape): Boolean =
      val elementPositionX = position._1
      val elementPositionY = position._2
      val mouseClickX = clickPosition._1
      val mouseClickY = clickPosition._2

      shape match
        case SimulationCircle(radius) =>
          val distanceX = mouseClickX - elementPositionX
          val distanceY = mouseClickY - elementPositionY

          (distanceX * distanceX + distanceY * distanceY) <= (radius * radius)

        case SimulationRectangle(width, height) =>
          val halfW = width / 2
          val halfH = height / 2

          mouseClickX >= (elementPositionX - halfW) && mouseClickX <= (elementPositionX + halfW) &&
            mouseClickY >= (elementPositionY - halfH) && mouseClickY <= (elementPositionY + halfH)

    def checkMouseHit(mouseClick: (Double, Double)): Boolean =
      clickable match
        case surface: MonadCoreSurface => checkGenericHit(surface.position, mouseClick, surface.shape)
        case entity: MonadCoreEntity => checkGenericHit(entity.position, mouseClick, entity.shape)


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
  def build()
           (
             using gameEngineRuntime: GameEngineRuntime,
             world: World,
             architect: ShapeArchitect,
             painter: Painter
           )
  : Either[BaseError, VBox] =

    val canvas = ResizableCanvas()
    val menusAnchor = Some(canvas)

    EntityContextMenu.attachTo(
      canvas = canvas,
      findEntityAt = (x, y) =>
        (for
          entities <- world.getAllEntities
          surfaces <- world.getAllSurfaces
          clickableElements = List.from(entities).appendedAll(surfaces)
        yield clickableElements.find(_.checkMouseHit((x, y)))) match 
          case Right(value) => value
          case Left(error) => 
            displayError(error)
            None
        ,
      buildMenuItems = {
        case entity: MonadCoreEntity => Seq(
          MenuButtonItem(s"Edit ${entity.id} Entity", () => SaveEntityFormDialog.show(
            props = SaveEntityFormDialogProps(
              title = "Entity Settings",
              anchorNode = menusAnchor,
              onSubmit = entity => onActionMakeSnapshot(SaveEntityCommand(entity), world.updateEntity),
              teams = getTeamsSafely(world),
              onError = displayError,
              entityToUpdate = Some(entity)
            )
          )),
          MenuButtonItem(s"Remove ${entity.id} Entity", () => onActionMakeSnapshot(entity.id, world.removeEntity))
        )
        case surface: MonadCoreSurface => Seq(
          MenuButtonItem(s"Edit ${surface.id} Surface", () => SaveSurfaceFormDialog.show(
            props = SaveSurfaceFormDialogProps(
              title = "Surface Settings",
              anchorNode = menusAnchor,
              onSubmit = surface => onActionMakeSnapshot(SaveSurfaceCommand(surface), world.updateSurface),
              onError = displayError,
              surfaceToUpdate = Some(surface)
            )
          )),
          MenuButtonItem(s"Remove ${surface.id} Surface", () => onActionMakeSnapshot(surface.id, world.removeSurface))
        )
      }
    )

    val onFrame: World => Unit = _ => ShapePainter.paint(canvas.graphicsContext2D)

    gameEngineRuntime.attach(onFrame)
    gameEngineRuntime.resetToSnapshot()

    val container = new VBox:
      children = Seq(canvas)
      style = PanelStyles.base

    VBox.setVgrow(canvas, Priority.Always)

    Right(container)