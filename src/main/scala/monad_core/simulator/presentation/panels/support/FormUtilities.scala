package monad_core.simulator.presentation.panels.support

import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.{Error, NotificationManager}

/**
 * Support Object that provides utilities for the forms handle by their stages/panels
 */
object FormUtilities:

  /**
   * Utilizes the
   * [[monad_core.simulator.presentation.components.NotificationManager NotificationManager]] to display the provided
   * error to the user.
   *
   * @see [[monad_core.simulator.presentation.components.NotificationManager NotificationManager]]
   * @param error the error that needs to be displayed by the snackbar
   */
  def displayError(error: BaseError): Unit =
    NotificationManager.show(error.message, Error)

  /**
   * Makes a snapshot upon action success, otherwise call the [[displayError()]] function to notify the user.
   *
   * The provided input is provided to the action, if Right is returned
   * [[monad_core.simulator.application.engine.GameEngineRuntime.createSnapshot GameEngineRuntime.createSnapshot()]] is
   * called.
   *
   * @param actionInput the input for the action function
   * @param action the action that after which a snapshot needs to be created
   * @param gameEngineRuntime the
   *                          [[monad_core.simulator.application.engine.GameEngineRuntime GameEngineRuntime]] utilized by
   *                          the Gui application
   * @tparam T the input type to the action
   */
  def onActionMakeSnapshot[T](
      actionInput: T
  )(action: T => Either[BaseError, Unit])(using
      gameEngineRuntime: GameEngineRuntime
  ): Unit =
    action(actionInput) match
      case Left(error) => displayError(error)
      case Right(_)    => gameEngineRuntime.createSnapshot()
