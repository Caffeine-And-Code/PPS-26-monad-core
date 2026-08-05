package monad_core.simulator

import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.resources.Image

case class ImageResourceNotFound(image: Image) extends BaseError(s"image ${image.fileName} not found")

case class CannotBuildButton(error: BaseError, buttonId: String) extends BaseError(s"$buttonId button cannot be built: \n   ${error.message}")

case class CannotBuildPanel(error: BaseError, panelId: String) extends BaseError(s"$panelId panel cannot be built: \n   ${error.message}")

case class CannotBuildStage(error: BaseError, stageId: String) extends BaseError(s"$stageId stage cannot be built: \n   ${error.message}")

case class StartupTimeout(seconds: Long) extends BaseError(s"JavaFX startup did not complete within ${seconds}s")

case class UnexpectedStartupFailure(cause: String) extends BaseError(s"unexpected failure during JavaFX startup: $cause")

case class CannotBuildDialog(cause: String, dialogId: String) extends BaseError(s"$dialogId cannot be built: \n $cause")

case class MissingKeyInFormError(key: String) extends BaseError(s"$key is not present in form field list")

case class InvalidNumericFormFieldError(key: String) extends BaseError(s"$key has an invalid value: Numeric value Required")

case class InvalidShapeFormFieldError(key: String) extends BaseError(s"$key has an invalid value: Invalid Shape Value")

case class TeamNotFoundDuringSelection(teamId: String) extends BaseError(s"$teamId was not found in teams")