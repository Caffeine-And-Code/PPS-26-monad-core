package monad_core.simulator.application.engine

import java.util.concurrent.atomic.AtomicReference
import monad_core.engine.core.GameLoop
import monad_core.engine.public_api.{EngineFacade, Painter}
import monad_core.simulator.application.engine.world.World
import scalafx.animation.AnimationTimer

final case class GameEngine(
                             init: World => Unit,
                             play: () => Unit,
                             pause: () => Unit,
                             reset: World => Unit,
                             dispose: () => Unit
                           )

object GameEngine:

  private[application] def tick(world: World, loop: GameLoop, currentTime: Long)(using painter: Painter): (World, GameLoop) =
    val (newState, newLoop) = EngineFacade.tick(loop, world.snapshot, currentTime)
    (World(newState), newLoop)

  def apply(onFrame: World => Unit)(using painter: Painter): GameEngine =
    val worldRef = new AtomicReference[World]()
    val loopRef = new AtomicReference[GameLoop](GameLoop())

    val timer: AnimationTimer = AnimationTimer { now =>
      val (newWorld, newLoop) = tick(worldRef.get(), loopRef.get(), now)
      worldRef.set(newWorld)
      loopRef.set(newLoop)
      onFrame(newWorld)
    }

    GameEngine(
      init = world => {
        worldRef.set(world)
        loopRef.set(GameLoop())
        timer.start()
      },
      play = () => loopRef.updateAndGet(_.start()),
      pause = () => loopRef.updateAndGet(_.stop()),
      reset = world => {
        worldRef.set(world)
        loopRef.set(GameLoop())
      },
      dispose = () => timer.stop()
    )
