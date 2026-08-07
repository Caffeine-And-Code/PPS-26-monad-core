package monad_core.simulator.application.engine

import monad_core.engine.public_api.Painter
import monad_core.simulator.application.engine.world.World

trait GameEngineRuntime:
  def start(): Unit
  def stop(): Unit
  def attach(renderer: World => Unit)(using Painter): Unit
  def isRunning: Boolean
  def createSnapshot() : Unit
  def resetToSnapshot() : Unit
  def initializeWorld(world: World) : Unit