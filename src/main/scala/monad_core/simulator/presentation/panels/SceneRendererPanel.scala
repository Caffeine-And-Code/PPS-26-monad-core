package monad_core.simulator.presentation.panels

import monad_core.engine.errors.EngineError
import monad_core.engine.public_api.Painter
import monad_core.simulator.application.engine.GameEngine
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.presentation.components.ResizableCanvas
import monad_core.simulator.presentation.painters.Drawer
import monad_core.simulator.presentation.panels.support.PanelStyles
import monad_core.simulator.presentation.panels.traits.SceneRendererPanelBuilder
import scalafx.scene.layout.{Priority, VBox}

object SceneRendererPanel extends SceneRendererPanelBuilder:
  def build(world: World): Either[EngineError, (VBox, GameEngine)] =
    val canvas = ResizableCanvas()
    val drawer = Drawer

    given Painter = drawer

    val onFrame: World => Unit = _ => drawer.flush(canvas.graphicsContext2D)
    val controller = GameEngine(onFrame)

    controller.init(world)

    val container = new VBox:
      children = Seq(canvas)
      style = PanelStyles.base

    VBox.setVgrow(canvas, Priority.Always)

    Right((container, controller))