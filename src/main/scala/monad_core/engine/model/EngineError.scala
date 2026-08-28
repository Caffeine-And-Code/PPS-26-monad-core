package monad_core.engine.model

trait EngineError(val message: String)

case class UnhandledStateType() extends EngineError("Unhandled state type")
