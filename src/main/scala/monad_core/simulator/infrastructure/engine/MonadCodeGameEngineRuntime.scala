package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.{GameLoop, Scene}
import monad_core.engine.core.GameLoop.*
import monad_core.engine.public_api.{EngineFacade, Painter}
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.World
import scalafx.animation.AnimationTimer

final class MonadCodeGameEngineRuntime extends GameEngineRuntime:
  private var gameLoop = GameLoop()
  private var currentWorld: Option[World] = None
  private var timer: Option[AnimationTimer] = None

  override def start(): Unit =
    gameLoop = gameLoop.start()

  override def stop(): Unit =
    gameLoop = gameLoop.stop()

  override def reset(world: World): Unit =
    gameLoop = GameLoop()
    currentWorld = Some(world)

  override def attach(renderer: World => Unit)(using painter: Painter): Unit =
    timer.foreach(_.stop())
    val animationTimer = AnimationTimer { currentTime =>
      currentWorld.foreach { world =>
        val (nextState, nextLoop) =
          EngineFacade.tick(gameLoop, world.scene, currentTime)

        (world, nextState) match
          case (monadCoreWorld: MonadCoreWorld, scene: Scene) =>
            monadCoreWorld.currentScene = scene
          case _ => ()

        gameLoop = nextLoop
        renderer(world)
      }
    }
    timer = Some(animationTimer)
    animationTimer.start()

  override def isRunning: Boolean = gameLoop.isRunning

object MonadCodeGameEngineRuntime:
  def apply(): MonadCodeGameEngineRuntime = new MonadCodeGameEngineRuntime
