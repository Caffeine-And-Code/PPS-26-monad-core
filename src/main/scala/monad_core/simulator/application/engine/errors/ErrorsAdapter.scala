package monad_core.simulator.application.engine.errors

import monad_core.engine.model.EngineError
import monad_core.simulator.errors.BaseError

/**
 * The [[monad_core.simulator.errors.BaseError BaseError]] used as a wrapper to all the
 * [[monad_core.engine.model.EngineError EngineError]]
 * @param engineError error to wrap
 */
case class EngineErrorAdapted(engineError: EngineError) extends BaseError(engineError.message)

/**
 * Adapter class from [[monad_core.engine.model.EngineError EngineError]] to
 * [[monad_core.simulator.errors.BaseError BaseError]]
 */
object ErrorsAdapter:

  extension (coreError: EngineError)

    /**
     * wrap an [[monad_core.engine.model.EngineError EngineError]] with a [[EngineErrorAdapted]] to obfuscate the usage of
     * EngineError from the simulator package
     * @return the [[monad_core.simulator.errors.BaseError BaseError]]
     */
    def adaptError(): BaseError =
      EngineErrorAdapted(coreError)

  extension [T](either: Either[EngineError, T])

    /**
     * utilizes the base adaptError to wrap any Either[EngineError, A]
     * 
     * @see [[adaptError()]]
     * @return
     */
    def adaptError(): Either[BaseError, T] =
      either.left.map(_.adaptError())
