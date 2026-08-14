package monad_core.simulator.presentation.components.ai

import scalafx.geometry.Insets
import scalafx.scene.layout.VBox

object MessageComponents:

  def Box(boxId: String): VBox =
    new VBox {
      id = boxId
      spacing = 20
      padding = Insets(0, 10, 20, 0)
    }
