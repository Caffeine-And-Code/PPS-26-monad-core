package monad_core.simulator.presentation.panels.traits

import monad_core.engine.simulator.Painter
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.application.engine.{GameEngineRuntime, ShapeArchitect}
import monad_core.simulator.errors.BaseError
import scalafx.scene.layout.VBox

trait SceneRendererPanelBuilder:

  def build()(using
      gameEngineRuntime: GameEngineRuntime,
      world: World,
      architect: ShapeArchitect,
      painter: Painter
  ): Either[BaseError, VBox]
