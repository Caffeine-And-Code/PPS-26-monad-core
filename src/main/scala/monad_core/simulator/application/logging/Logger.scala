package monad_core.simulator.application.logging

trait Logger:
  def info(message: String): Unit

  def trace(message: String): Unit = ()
