package monad_core.simulator.application.engine

import scala.concurrent.Future

trait GameEngineRuntime:

  def start(): Unit
  def stop(): Unit
  def reset(word: Word): Unit
  def init(
          initialWord: Word,
          renderer: Word => Word
          ): Future[Unit]
