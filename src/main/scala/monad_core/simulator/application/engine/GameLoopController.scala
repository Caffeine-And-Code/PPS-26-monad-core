package monad_core.simulator.application.engine

import monad_core.simulator.application.engine.world.World

trait GameLoopController:
  def init(world: World): Unit
  def play(): Unit
  def pause(): Unit
  def reset(world: World): Unit