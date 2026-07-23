package monad_core.simulator.infrastructure.fx

import monad_core.engine.core.GameLoop
import monad_core.simulator.application.engine.{GameEngineRuntime, GameLoopController}
import monad_core.simulator.application.engine.world.World
import scalafx.animation.AnimationTimer

final class GameLoopDriver(runtime: GameEngineRuntime, onFrame: World => Unit)
  extends GameLoopController:

  private var world: World = _
  private var loop: GameLoop = GameLoop()

  private val timer: AnimationTimer = AnimationTimer { now =>
    val (newWorld, newLoop) = runtime.tick(world, loop, now)
    world = newWorld
    loop = newLoop
    onFrame(newWorld)
  }

  def init(initialWorld: World): Unit =
    world = initialWorld
    loop = GameLoop()
    timer.start()

  def play(): Unit =
    loop = loop.start()

  def pause(): Unit =
    loop = loop.stop()

  def reset(newWorld: World): Unit =
    world = newWorld
    loop = GameLoop()