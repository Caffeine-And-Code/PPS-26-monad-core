package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.GameLoop.*
import monad_core.engine.core.{GameLoop, Scene}
import monad_core.engine.public_api.{EngineFacade, Painter}
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.{SaveEntityCommand, World}
import monad_core.simulator.domain.engine.MonadCoreEntity
import monad_core.simulator.domain.engine.MonadCoreShape.SimulationCircle
import scalafx.animation.AnimationTimer

final class MonadCoreGameEngineRuntime extends GameEngineRuntime:
  private var gameLoop = GameLoop()
  private var currentWorld: Option[World] = None
  private var timer: Option[AnimationTimer] = None
  private var currentSnapshot: Option[Scene] = None

  override def start(): Unit =
    gameLoop = gameLoop.start()

  override def stop(): Unit =
    gameLoop = gameLoop.stop()

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

  override def createSnapshot(): Unit =
    currentSnapshot = currentWorld.map(_.scene)

  override def resetToSnapshot(): Unit =
    (currentWorld, currentSnapshot) match
      case (Some(monadCoreWorld: MonadCoreWorld), Some(scene)) =>
        monadCoreWorld.currentScene = scene
      case _ => ()
    gameLoop = GameLoop()

  override def initializeWorld(world: World): Unit =
    world.createEntity(
      SaveEntityCommand(
        MonadCoreEntity(
          id = "starter",
          position = (15, 15),
          shape = SimulationCircle(15)
        )
      )
    )
    currentWorld = Some(world)

object MonadCoreGameEngineRuntime:
  def apply(): MonadCoreGameEngineRuntime = new MonadCoreGameEngineRuntime