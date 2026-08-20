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
import scalafx.animation.AnimationTimer
import scalafx.scene.control.ContextMenu
import scalafx.scene.layout.{Priority, VBox}

object SceneRendererPanel extends SceneRendererPanelBuilder:

  private case class SceneRendererViewModel(world: World, gameEngineRuntime: GameEngineRuntime)

  extension (viewModel: SceneRendererViewModel)
    private def error: Option[BaseError] = viewModel.gameEngineRuntime.getError

    private def findClickableAt(x: Double, y: Double): Option[Clickable] =
      val clickableElements: List[Clickable] =
        viewModel.world.getAllEntities ++ viewModel.world.getAllSurfaces

      clickableElements.find(_.checkMouseHit((x, y)))

    private def onSaveEntity(entity: Entity): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(SaveEntityCommand(entity), viewModel.world.updateEntity)

    private def onRemoveEntity(entity: Entity): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(entity.id, id => viewModel.world.removeEntity(id.value))

    private def onSaveSurface(surface: Surface): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(SaveSurfaceCommand(surface), viewModel.world.updateSurface)

    private def onRemoveSurface(surface: Surface): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(surface.id, id => viewModel.world.removeSurface(id.value))

    private def startRendering(onFrame: World => Unit)(using Painter): AnimationTimer =
      viewModel.gameEngineRuntime.resetToSnapshot()
      val animationTimer = AnimationTimer { currentTime =>
        viewModel.gameEngineRuntime.tick(currentTime)(onFrame)
      }
      animationTimer.start()

      animationTimer

  def build()(using
      gameEngineRuntime: GameEngineRuntime,
      world: World,
      architect: ShapeArchitect,
      painter: Painter
  ): Either[BaseError, VBox] =

    val viewModel = SceneRendererViewModel(world, gameEngineRuntime)

    viewModel.error match
      case Some(err) => Left(err)
      case None =>
        val canvas      = ResizableCanvas()
        val menusAnchor = Some(canvas)

        def resizeWorld(): Unit =
          val width  = canvas.width.value
          val height = canvas.height.value

          if width > 0 && height > 0 then gameEngineRuntime.resize(width, height)

        canvas.width.onChange {
          resizeWorld()
        }

        canvas.height.onChange {
          resizeWorld()
        }

        resizeWorld()

        RightClickContextMenu.attachTo(
          canvas = canvas,
          findElementAt = viewModel.findClickableAt,
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
                        onSubmit = viewModel.onSaveEntity,
                        teams = world.getAllTeams,
                        onError = displayError,
                        entityToUpdate = Some(entity)
                      )
                    )
                ),
                MenuButtonItem(
                  s"Remove ${entity.id} Entity",
                  () => viewModel.onRemoveEntity(entity)
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
                        onSubmit = viewModel.onSaveSurface,
                        onError = displayError,
                        surfaceToUpdate = Some(surface)
                      )
                    )
                ),
                MenuButtonItem(
                  s"Remove ${surface.id} Surface",
                  () => viewModel.onRemoveSurface(surface)
                )
              )
          }
        )

        viewModel.startRendering(_ => ShapePainter.paint(canvas.graphicsContext2D))

        val container = new VBox:
          children = Seq(canvas)
          style = PanelStyles.sceneRenderer

        VBox.setVgrow(canvas, Priority.Always)

        Right(container)

type Clickable = Surface | Entity

private object MouseHitDetector:

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
        case surface: Surface => checkGenericHit(surface.position, mouseClick, surface.shape)
        case entity: Entity   => checkGenericHit(entity.position, mouseClick, entity.shape)

private object RightClickContextMenu:

  def attachTo(
      canvas: javafx.scene.canvas.Canvas,
      findElementAt: (Double, Double) => Option[Clickable],
      buildMenuItems: Clickable => Seq[MenuButtonItem]
  ): Unit =

    val contextMenu = new ContextMenu:
      styleClass += "app-context-menu"

    canvas.setOnMouseClicked((e: MouseEvent) =>
      contextMenu.hide()

      if e.getButton == MouseButton.SECONDARY then
        val localX = e.getX
        val localY = e.getY

        findElementAt(localX, localY) match
          case Some(entity) =>
            contextMenu.items.clear()
            contextMenu.items ++= buildMenuItems(entity).map(_.toMenuItem)
            contextMenu.show(canvas, e.getScreenX, e.getScreenY)

          case None =>
            contextMenu.hide()
    )
