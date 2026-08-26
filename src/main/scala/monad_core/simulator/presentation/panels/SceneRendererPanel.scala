package monad_core.simulator.presentation.panels

import javafx.scene.input.{MouseButton, MouseEvent}
import monad_core.engine.model.{Entity, Shape2D, Surface, Vector2D}
import monad_core.engine.simulator.{DrawCommand, Painter}
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.{SaveEntityCommand, SaveSurfaceCommand, World}
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
import scalafx.beans.property.BooleanProperty
import scalafx.scene.control.ContextMenu
import scalafx.scene.layout.{Priority, VBox}

/**
 * SceneRendererPanel concrete builder.
 *
 * It handles the rendering of the current [[World]] and the user interaction
 * with its clickable elements.
 */
object SceneRendererPanel extends SceneRendererPanelBuilder:

  /**
   * ViewModel used as state of the [[SceneRendererPanel]] component.
   *
   * @param world current [[World]] displayed by the panel
   * @param gameEngineRuntime [[GameEngineRuntime]] used to handle and mutate the world
   */
  private case class SceneRendererViewModel(world: World, gameEngineRuntime: GameEngineRuntime)

  extension (viewModel: SceneRendererViewModel)

    /**
     * Returns the current error of the [[GameEngineRuntime]], if present.
     *
     * @return `Some(BaseError)` if the runtime currently contains an error,
     *         `None` otherwise
     */
    private def error: Option[BaseError] = viewModel.gameEngineRuntime.getError

    /**
     * Searches for a clickable world element at the provided coordinates.
     *
     * Both [[Entity]] and [[Surface]] elements are considered clickable.
     * The actual hit detection is delegated to [[MouseHitDetector.checkMouseHit]].
     *
     * @param x horizontal coordinate of the mouse click
     * @param y vertical coordinate of the mouse click
     * @return the first [[Clickable]] element found at the provided position,
     *         or `None` if no element has been hit
     */
    private def findClickableAt(x: Double, y: Double): Option[Clickable] =
      val clickableElements: List[Clickable] =
        viewModel.world.getAllEntities ++ viewModel.world.getAllSurfaces

      clickableElements.find(_.checkMouseHit((x, y)))

    /**
     * Wraps the [[World.updateEntity]] function to handle and display a possible error returned by it.
     *
     * By doing so [[onActionMakeSnapshot]] is used to make a snapshot on `Right` result,
     * or display the error on `Left`.
     *
     * @see [[onActionMakeSnapshot]], [[World.updateEntity]]
     * @param entity the entity that needs to be updated
     */
    private def onSaveEntity(entity: Entity): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(SaveEntityCommand(entity))(viewModel.world.updateEntity)

    /**
     * Wraps the [[World.removeEntity]] function to handle and display a possible error returned by it.
     *
     * By doing so [[onActionMakeSnapshot]] is used to make a snapshot on `Right` result,
     * or display the error on `Left`.
     *
     * @see [[onActionMakeSnapshot]], [[World.removeEntity]]
     * @param entity the entity that needs to be removed
     */
    private def onRemoveEntity(entity: Entity): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(entity.id)(id => viewModel.world.removeEntity(id.value))

    /**
     * Wraps the [[World.updateSurface]] function to handle and display a possible error returned by it.
     *
     * By doing so [[onActionMakeSnapshot]] is used to make a snapshot on `Right` result,
     * or display the error on `Left`.
     *
     * @see [[onActionMakeSnapshot]], [[World.updateSurface]]
     * @param surface the surface that needs to be updated
     */
    private def onSaveSurface(surface: Surface): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(SaveSurfaceCommand(surface))(viewModel.world.updateSurface)

    /**
     * Wraps the [[World.removeSurface]] function to handle and display a possible error returned by it.
     *
     * By doing so [[onActionMakeSnapshot]] is used to make a snapshot on `Right` result,
     * or display the error on `Left`.
     *
     * @see [[onActionMakeSnapshot]], [[World.removeSurface]]
     * @param surface the surface that needs to be removed
     */
    private def onRemoveSurface(surface: Surface): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(surface.id)(id => viewModel.world.removeSurface(id.value))

    /**
     * Starts the rendering loop of the [[GameEngineRuntime]].
     *
     * Before starting the animation, the runtime is restored to its current snapshot.
     * At each animation frame the runtime tick is executed and the provided callback
     * receives the current [[World]] together with the generated vector of [[DrawCommand]].
     *
     * @param onFrame callback executed by the runtime at each animation frame
     * @param painter [[Painter]] used by the game engine during rendering
     * @return the started [[AnimationTimer]]
     */
    private def startRendering(
        onFrame: (World, Vector[DrawCommand]) => Unit
    )(using painter: Painter): AnimationTimer =
      viewModel.gameEngineRuntime.resetToSnapshot()
      val animationTimer = AnimationTimer { currentTime =>
        viewModel.gameEngineRuntime.tick(currentTime)(onFrame)
      }
      animationTimer.start()

      animationTimer

  /**
   * Imperative shell that constructs the panel.
   *
   * It creates a resizable canvas used to render the current world and keeps
   * the [[GameEngineRuntime]] dimensions synchronized with the canvas dimensions.
   *
   * A right-click context menu is attached to the canvas, allowing the user to:
   * - edit or remove an [[Entity]]
   * - edit or remove a [[Surface]]
   *
   * These interactions are available only while the engine is not running.
   *
   * Finally, the rendering loop is started and each generated sequence of
   * [[DrawCommand]]s is painted on the canvas through [[ShapePainter]].
   *
   * @param isEngineRunning [[BooleanProperty]] representing if the engine is currently running or not
   * @param gameEngineRuntime [[GameEngineRuntime]] used to handle the world
   * @param world current [[World]] rendered by the panel
   * @param painter [[Painter]] used by the game engine to generate the drawing commands
   * @return `Left(BaseError)` if the runtime already contains an error
   *
   *         `Right(VBox)` if the panel is built, the [[VBox]] is the panel
   */
  def build(isEngineRunning: BooleanProperty)(using
      gameEngineRuntime: GameEngineRuntime,
      world: World,
      painter: Painter
  ): Either[BaseError, VBox] =

    val viewModel = SceneRendererViewModel(world, gameEngineRuntime)

    viewModel.error match
      case Some(err) => Left(err)
      case None =>
        val canvas      = ResizableCanvas()
        val menusAnchor = Some(canvas)

        /**
         * Keeps the game engine world dimensions synchronized with the current
         * canvas dimensions.
         */
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
                    ),
                  isDisabled = isEngineRunning
                ),
                MenuButtonItem(
                  s"Remove ${entity.id} Entity",
                  () => viewModel.onRemoveEntity(entity),
                  isDisabled = isEngineRunning
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
                    ),
                  isDisabled = isEngineRunning
                ),
                MenuButtonItem(
                  s"Remove ${surface.id} Surface",
                  () => viewModel.onRemoveSurface(surface),
                  isDisabled = isEngineRunning
                )
              )
          },
          isEngineRunning = isEngineRunning
        )

        viewModel.startRendering((_, commands) =>
          ShapePainter.paint(canvas.graphicsContext2D, commands)
        )

        val container = new VBox:
          children = Seq(canvas)
          style = PanelStyles.sceneRenderer

        VBox.setVgrow(canvas, Priority.Always)

        Right(container)

/**
 * Type representing every world element which can be selected through mouse interaction.
 */
type Clickable = Surface | Entity

/**
 * Utility responsible for detecting mouse interactions with [[Clickable]] elements.
 *
 * It performs shape-based hit detection using the position and geometry of
 * either an [[Entity]] or a [[Surface]].
 */
private object MouseHitDetector:

  extension (clickable: Clickable)

    /**
     * Checks whether a mouse position lies inside the provided shape.
     *
     * For a [[Shape2D.Circle]] the Euclidean distance from its center is used,
     * while for a [[Shape2D.Rectangle]] the click is checked against its bounds.
     *
     * @param position center position of the element
     * @param clickPosition position of the mouse click
     * @param shape shape of the clickable element
     * @return `true` if the mouse click lies inside the shape, `false` otherwise
     */
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

    /**
     * Checks whether the provided mouse position hits the current [[Clickable]] element.
     *
     * The element position and shape are extracted according to its concrete
     * [[Entity]] or [[Surface]] type and passed to the generic hit detection logic.
     *
     * @see [[checkGenericHit]]
     * @param mouseClick position of the mouse click
     * @return `true` if the clickable element has been hit, `false` otherwise
     */
    def checkMouseHit(mouseClick: (Double, Double)): Boolean =
      clickable match
        case surface: Surface => checkGenericHit(surface.position, mouseClick, surface.shape)
        case entity: Entity   => checkGenericHit(entity.position, mouseClick, entity.shape)

/**
 * Utility responsible for attaching and handling a right-click context menu on a canvas.
 */
private object RightClickContextMenu:

  /**
   * Attaches a context menu to the provided canvas.
   *
   * On a secondary mouse click, when the engine is not running, it searches
   * for a [[Clickable]] element at the mouse position. If an element is found,
   * the context menu is populated using [[buildMenuItems]] and displayed at
   * the current screen coordinates.
   *
   * Any previously visible context menu is hidden before processing a new click.
   *
   * @param canvas canvas on which the context menu is attached
   * @param findElementAt function used to find a [[Clickable]] element at the provided coordinates
   * @param buildMenuItems function used to construct the menu items for the selected element
   * @param isEngineRunning [[BooleanProperty]] representing if the engine is currently running or not
   */
  def attachTo(
      canvas: javafx.scene.canvas.Canvas,
      findElementAt: (Double, Double) => Option[Clickable],
      buildMenuItems: Clickable => Seq[MenuButtonItem],
      isEngineRunning: BooleanProperty
  ): Unit =

    val contextMenu = new ContextMenu:
      styleClass += "app-context-menu"

    canvas.setOnMouseClicked((e: MouseEvent) =>
      contextMenu.hide()

      if e.getButton == MouseButton.SECONDARY && !isEngineRunning.value then
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
