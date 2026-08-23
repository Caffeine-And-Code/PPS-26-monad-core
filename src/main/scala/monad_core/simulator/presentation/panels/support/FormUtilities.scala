package monad_core.simulator.presentation.panels.support

import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.{Error, NotificationManager}

object FormUtilities:

  def displayError(error: BaseError): Unit =
    NotificationManager.show(error.message, Error)

  def onActionMakeSnapshot[T](submitResult: T, action: T => Either[BaseError, Unit])(using
      gameEngineRuntime: GameEngineRuntime
  ): Unit =
    action(submitResult) match
      case Left(error) => displayError(error)
      case Right(_)    => gameEngineRuntime.createSnapshot()
