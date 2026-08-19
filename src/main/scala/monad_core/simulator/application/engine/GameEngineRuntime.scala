package monad_core.simulator.application.engine

import monad_core.engine.simulator.Painter
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.errors.BaseError

trait GameEngineRuntime extends EngineControl:
  def tick(currentTime: Long)(renderer: World => Unit)(using Painter): Unit
  def createSnapshot(): Unit
  def resetToSnapshot(): Unit
  def initializeWorld(world: World, withDefaultEntity: Boolean = true): Either[BaseError, Unit]
  def getError: Option[BaseError]
