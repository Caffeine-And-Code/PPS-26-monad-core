package monad_core.simulator.application.engine

import monad_core.engine.core.GameLoop
import monad_core.engine.public_api.{EngineFacade, Painter}
import monad_core.simulator.application.engine.world.World

trait GameEngineRuntime:
  def tick(world: World, loop: GameLoop, currentTime: Long): (World, GameLoop)

object GameEngineRuntime:
  def apply()(using painter: Painter): GameEngineRuntime =
    (world, loop, currentTime) =>
      val (newState, newLoop) = EngineFacade.tick(loop, world.snapshot, currentTime)
      (World(newState), newLoop)