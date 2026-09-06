package monad_core.engine.model

/**
 * Base type for errors produced by the engine domain.
 *
 * @param message human-readable description of the failure
 */
trait EngineError(val message: String)

/** Indicates that an engine state has an unhandled state type. */
case class UnhandledStateType() extends EngineError("Unhandled state type")
