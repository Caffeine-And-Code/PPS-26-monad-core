package monad_core.simulator.presentation.panels.traits

import monad_core.engine.simulator.Painter
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.errors.BaseError
import scalafx.beans.property.BooleanProperty
import scalafx.scene.layout.VBox

trait SceneRendererPanelBuilder:

  def build(isEngineRunning: BooleanProperty)(using
      gameEngineRuntime: GameEngineRuntime,
      world: World,
      painter: Painter
  ): Either[BaseError, VBox]
