package monad_core.simulator.application.engine

trait EngineControl:
  def start(): Unit

  def stop(): Unit

  def isRunning: Boolean
