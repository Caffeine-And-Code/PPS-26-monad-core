package monad_core.simulator.application.engine.errors

import monad_core.engine.model.EngineError
import monad_core.simulator.errors.BaseError

/**
 * The [[BaseError]] used as a wrapper to all the [[EngineError]] 
 * @param engineError error to wrap
 */
case class EngineErrorAdapted(engineError: EngineError) extends BaseError(engineError.message)

/**
 * Adapter class from [[EngineError]] to [[BaseError]]
 */
object ErrorsAdapter:

  extension (coreError: EngineError)

    /**
     * wrap an [[EngineError]] with a [[EngineErrorAdapted]] to obfuscate the usage of 
     * EngineError from the simulator package
     * @return the [[BaseError]]
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
