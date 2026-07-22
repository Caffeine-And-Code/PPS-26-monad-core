package monad_core.simulator.application.engine

import scala.concurrent.Future

trait GameEngineRuntime:

  def start(): Unit
  def stop(): Unit
  def reset(world: World): Unit
  def init(
            initialWorld: World,
            renderer: World => World
          ): Future[Unit]
