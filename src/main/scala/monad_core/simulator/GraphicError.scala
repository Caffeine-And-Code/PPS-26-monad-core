package monad_core.simulator

import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.resources.Image

/**
 * Indicates that an image resource cannot be resolved from the configured classpath.
 *
 * @param image
 *   missing image descriptor
 */
case class ImageResourceNotFound(image: Image)
    extends BaseError(s"image ${image.fileName} not found")

/**
 * Wraps an error raised while building a graphical button.
 *
 * @param error
 *   underlying build failure
 * @param buttonId
 *   identifier of the button being built
 */
case class CannotBuildButton(error: BaseError, buttonId: String)
    extends BaseError(s"$buttonId button cannot be built: \n   ${error.message}")

/**
 * Wraps an error raised while building a panel.
 *
 * @param error
 *   underlying build failure
 * @param panelId
 *   identifier of the panel being built
 */
case class CannotBuildPanel(error: BaseError, panelId: String)
    extends BaseError(s"$panelId panel cannot be built: \n   ${error.message}")

/**
 * Wraps an error raised while building a stage.
 *
 * @param error
 *   underlying build failure
 * @param stageId
 *   identifier of the stage being built
 */
case class CannotBuildStage(error: BaseError, stageId: String)
    extends BaseError(s"$stageId stage cannot be built: \n   ${error.message}")

/**
 * Indicates that JavaFX initialization exceeded its time limit.
 *
 * @param seconds
 *   number of seconds waited before timing out
 */
case class StartupTimeout(seconds: Long)
    extends BaseError(s"JavaFX startup did not complete within ${seconds}s")

/**
 * Describes an unexpected failure raised while starting JavaFX.
 *
 * @param cause
 *   textual description of the original failure
 */
case class UnexpectedStartupFailure(cause: String)
    extends BaseError(s"unexpected failure during JavaFX startup: $cause")

/**
 * Describes a failure raised while constructing a dialog.
 *
 * @param cause
 *   textual description of the original failure
 * @param dialogId
 *   identifier of the dialog being built
 */
case class CannotBuildDialog(cause: String, dialogId: String)
    extends BaseError(s"$dialogId cannot be built: \n $cause")

/**
 * Indicates that a required field identifier is missing from submitted values.
 *
 * @param key
 *   missing field identifier
 */
case class MissingKeyInFormError(key: String)
    extends BaseError(s"$key is not present in form field list")

/**
 * Indicates that a form field cannot be converted to the required numeric type.
 *
 * @param key
 *   identifier of the invalid field
 */
case class InvalidNumericFormFieldError(key: String)
    extends BaseError(s"$key has an invalid value: Numeric value Required")

/**
 * Indicates that a form field contains an unsupported shape label.
 *
 * @param key
 *   identifier of the invalid field
 */
case class InvalidShapeFormFieldError(key: String)
    extends BaseError(s"$key has an invalid value: Invalid Shape Value")

/**
 * Indicates that a submitted team selection no longer matches an available team.
 *
 * @param teamId
 *   unresolved team identifier
 */
case class TeamNotFoundDuringSelection(teamId: String)
    extends BaseError(s"$teamId was not found in teams")
