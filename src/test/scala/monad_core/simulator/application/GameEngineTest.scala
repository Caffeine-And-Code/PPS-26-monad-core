package monad_core.simulator.application

import monad_core.engine.core.LoopMode.{EditMode, SimulationMode}
import monad_core.engine.core.{GameLoop, Scene}
import monad_core.engine.public_api.Painter
import monad_core.simulator.application.engine.GameEngine
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.presentation.painters.Drawer
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite

class GameEngineTest extends AnyFunSuite:

  given Painter = Drawer

  private def emptyWorld: World = World(Scene())

  test("tick in edit mode does not run physics and only refreshes lastTime") {
    val loop = GameLoop().value
    val world = emptyWorld

    val (newWorld, newLoop) = GameEngine.tick(world, loop, currentTime = 1_000L)

    assert(!newLoop.isRunning)
    assert(newLoop.mode == EditMode)
    assert(newLoop.lastTime == 1_000L)
    assert(newLoop.accumulator == loop.accumulator)
    assert(newWorld.snapshot == world.snapshot)
  }

  test("tick in simulation mode below tick time only accumulates elapsed time") {
    val loop = GameLoop().value.start()
    val elapsed = loop.tickTime - 1

    val (_, newLoop) = GameEngine.tick(emptyWorld, loop, currentTime = elapsed)

    assert(newLoop.isRunning)
    assert(newLoop.accumulator == elapsed)
    assert(newLoop.lastTime == elapsed)
  }

  test("tick in simulation mode consumes exactly one fixed update when elapsed time equals tick time") {
    val loop = GameLoop().value.start()

    val (_, newLoop) = GameEngine.tick(emptyWorld, loop, currentTime = loop.tickTime)

    assert(newLoop.accumulator == 0L)
    assert(newLoop.lastTime == loop.tickTime)
  }

  test("tick clamps elapsed time to maxFrameTime, avoiding the spiral of death") {
    val loop = GameLoop().value.start()
    val farInTheFuture = loop.maxFrameTime * 2

    val (_, newLoop) = GameEngine.tick(emptyWorld, loop, currentTime = farInTheFuture)

    val stepsConsumed = loop.maxFrameTime / loop.tickTime
    val expectedAccumulator = loop.maxFrameTime - stepsConsumed * loop.tickTime

    assert(newLoop.accumulator == expectedAccumulator)
    assert(newLoop.lastTime == farInTheFuture)
  }

  test("tick returns to edit-mode behaviour once the loop is stopped") {
    val running = GameLoop().value.start()
    val (_, afterOneStep) = GameEngine.tick(emptyWorld, running, currentTime = running.tickTime)
    val stopped = afterOneStep.stop()

    val (_, newLoop) = GameEngine.tick(emptyWorld, stopped, currentTime = stopped.lastTime + 1_000L)

    assert(!newLoop.isRunning)
    assert(newLoop.mode == EditMode)
    assert(newLoop.lastTime == stopped.lastTime + 1_000L)
  }
