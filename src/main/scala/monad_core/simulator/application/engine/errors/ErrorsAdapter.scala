package monad_core.simulator.application.engine.errors

import monad_core.engine.model.EngineError
import monad_core.simulator.errors.BaseError

case class EngineErrorAdapted(engineError: EngineError) extends BaseError(engineError.message)

object ErrorsAdapter:

  extension (coreError: EngineError)

    def adaptError(): BaseError =
      EngineErrorAdapted(coreError)

  extension [T](either: Either[EngineError, T])

    def adaptError(): Either[BaseError, T] =
      either.left.map(_.adaptError())
