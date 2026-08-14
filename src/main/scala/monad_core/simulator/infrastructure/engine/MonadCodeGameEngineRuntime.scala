package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.GameLoop.*
import monad_core.engine.core.{GameLoop, RendererManager, Scene}
import monad_core.engine.errors.EngineError
import monad_core.engine.physics.core.PhysicsManager
import monad_core.engine.public_api.{EngineFacade, Painter}
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.World
import scalafx.animation.AnimationTimer

final class MonadCodeGameEngineRuntime extends GameEngineRuntime:
  private var gameLoop                      = GameLoop.default()
  private var currentWorld: Option[World]   = None
  private var timer: Option[AnimationTimer] = None
  private var error: Option[EngineError]    = None

  given physics: PhysicsManager        = PhysicsManager.default()
  given renderer: RendererManager.type = RendererManager

  override def start(): Unit =
    gameLoop = gameLoop.start()

  override def stop(): Unit =
    gameLoop = gameLoop.stop()

  override def reset(world: World): Unit =
    gameLoop = GameLoop.default()
    currentWorld = Some(world)
    error = None

  override def attach(renderer: World => Unit)(using painter: Painter): Unit =
    timer.foreach(_.stop())
    val animationTimer = AnimationTimer { currentTime =>
      currentWorld.foreach { world =>
        EngineFacade.tick(gameLoop, world.scene, currentTime) match
          case Right((nextState, nextLoop)) =>
            (world, nextState) match
              case (monadCoreWorld: MonadCoreWorld, scene: Scene) =>
                monadCoreWorld.currentScene = scene
              case _ => ()

            gameLoop = nextLoop
            renderer(world)

          case Left(engineError) =>
            error = Some(engineError)
            stop()
      }
    }
    timer = Some(animationTimer)
    animationTimer.start()

  override def isRunning: Boolean = gameLoop.isRunning

  override def getError: Option[EngineError] = error

object MonadCodeGameEngineRuntime:
  def apply(): MonadCodeGameEngineRuntime = new MonadCodeGameEngineRuntime
