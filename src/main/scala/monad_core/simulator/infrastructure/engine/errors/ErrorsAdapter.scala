package monad_core.simulator.infrastructure.engine.errors

import monad_core.engine.errors.EngineError
import monad_core.simulator.errors.BaseError

private[engine] case class EngineErrorAdapted(engineError: EngineError)
  extends BaseError(engineError.message)

object ErrorsAdapter:
  extension (coreError: EngineError)
    def adapt(): BaseError =
      EngineErrorAdapted(coreError)

  extension [T](either: Either[EngineError, T])
    def adapt(): Either[BaseError, T] =
      either.left.map(_.adapt())