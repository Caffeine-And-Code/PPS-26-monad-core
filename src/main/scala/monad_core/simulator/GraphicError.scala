package monad_core.simulator

import monad_core.engine.errors.EngineError
import monad_core.simulator.presentation.resources.Image

case class ImageResourceNotFound(image: Image) extends EngineError(s"image ${image.fileName} not found")

case class CannotBuildButton(error: EngineError, buttonId: String) extends EngineError(s"$buttonId button cannot be built: \n   ${error.message}")

case class CannotBuildPanel(error: EngineError, panelId: String) extends EngineError(s"$panelId panel cannot be built: \n   ${error.message}")

case class CannotBuildStage(error: EngineError, stageId: String) extends EngineError(s"$stageId stage cannot be built: \n   ${error.message}")

case class StartupTimeout(seconds: Long) extends EngineError(s"JavaFX startup did not complete within ${seconds}s")

case class UnexpectedStartupFailure(cause: String) extends EngineError(s"unexpected failure during JavaFX startup: $cause")

case class CannotBuildDialog(cause: String, dialogId: String) extends EngineError(s"$dialogId cannot be built: \n $cause")

case class MissingKeyInFormError(key: String) extends EngineError(s"$key is not present in form field list")

case class InvalidNumericFormFieldError(key: String) extends EngineError(s"$key has an invalid value: Numeric value Required")

case class InvalidShapeFormFieldError(key: String) extends EngineError(s"$key has an invalid value: Invalid Shape Value")