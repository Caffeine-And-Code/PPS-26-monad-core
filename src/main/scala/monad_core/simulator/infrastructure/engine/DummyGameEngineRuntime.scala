package monad_core.simulator.infrastructure.engine

import monad_core.simulator.application.engine.{GameEngineRuntime, Word}

import scala.concurrent.Future

case class DummyGameEngineRuntime(
  var isRunning: Boolean = false
) extends GameEngineRuntime:

  override def start(): Unit =
    isRunning = true

  override def stop(): Unit =
    isRunning = false

  override def reset(word: Word): Unit = ???

  override def init(
    initialWord: Word,
    renderer: Word => Word
  ): Future[Unit] = ???
