package monad_core.graphics

import monad_core.engine.errors.EngineError
import monad_core.graphics.resources.Image

case class ImageResourceNotFound(image: Image) extends EngineError(s"image ${image.fileName} not found")

case class CannotBuildButton(error: EngineError, buttonId: String) extends EngineError(s"$buttonId button cannot be built: \n   ${error.message}")
case class CannotBuildPanel(error: EngineError, panelId: String) extends EngineError(s"$panelId panel cannot be built: \n   ${error.message}")
case class CannotBuildStage(error: EngineError, stageId: String) extends EngineError(s"$stageId stage cannot be built: \n   ${error.message}")
