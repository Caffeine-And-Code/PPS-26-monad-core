package monad_core.simulator.presentation.panels.traits

import monad_core.simulator.application.engine.world.World
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.resources.ImageConfigRecord
import scalafx.beans.property.BooleanProperty
import scalafx.scene.layout.VBox

trait GameEngineModePanelBuilder:
  def build(
             imageConfig: ImageConfigRecord,
             onModeChange : Boolean => Unit,
             onStopClick: () => Unit,
             isEngineRunning: BooleanProperty,
           )(using world: World): Either[BaseError, VBox]
