package monad_core.simulator.infrastructure.engine

import monad_core.simulator.application.engine.EngineControl

final class HeadlessEngineControl extends EngineControl:
  private var running = false

  override def start(): Unit =
    running = true

  override def stop(): Unit =
    running = false

  override def isRunning: Boolean = running

object HeadlessEngineControl:
  def apply(): HeadlessEngineControl = new HeadlessEngineControl
