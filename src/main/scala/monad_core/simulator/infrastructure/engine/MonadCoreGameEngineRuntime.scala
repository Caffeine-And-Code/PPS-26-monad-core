package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.GameLoop.*
import monad_core.engine.core.{GameLoop, Scene}
import monad_core.engine.public_api.{EngineFacade, Painter}
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.{SaveEntityCommand, World}
import monad_core.simulator.domain.engine.{MonadCoreEntity, MonadCoreScene}
import monad_core.simulator.domain.engine.MonadCoreShape.SimulationCircle
import monad_core.simulator.infrastructure.engine.translators.SceneTranslator.*
import scalafx.animation.AnimationTimer

final class MonadCoreGameEngineRuntime extends GameEngineRuntime:
  private val lock = new Object
  private var gameLoop = GameLoop()
  private var currentWorld: Option[World] = None
  private var timer: Option[AnimationTimer] = None
  private var currentSnapshot: Option[MonadCoreScene] = None

  override def start(): Unit =
    lock.synchronized:
      gameLoop = gameLoop.start()

  override def stop(): Unit =
    lock.synchronized:
      gameLoop = gameLoop.stop()

  override def attach(renderer: World => Unit)(using painter: Painter): Unit =
    timer.foreach(_.stop())
    val animationTimer = AnimationTimer { currentTime =>
      lock.synchronized:
        currentWorld.foreach { world =>
          val convertedState = world.scene.toEngineModel match
            case Right(scene) => scene
            // This will likely never happen thanks to the checks done upon creation
            // of single entity,surface and team
            case _ => Scene()

          val (nextState, nextLoop) =
            EngineFacade.tick(gameLoop, convertedState, currentTime)

          (world, nextState) match
            case (monadCoreWorld: MonadCoreWorld, scene: Scene) =>
              monadCoreWorld.currentScene = scene.toSimulationScene
            case _ => ()

          gameLoop = nextLoop
          renderer(world)
        }
    }
    timer = Some(animationTimer)
    animationTimer.start()

  override def isRunning: Boolean =
    lock.synchronized(gameLoop.isRunning)

  override def createSnapshot(): Unit =
    lock.synchronized:
      currentSnapshot = currentWorld.map(_.scene)

  override def resetToSnapshot(): Unit =
    lock.synchronized:
      (currentWorld, currentSnapshot) match
        case (Some(monadCoreWorld: MonadCoreWorld), Some(scene)) =>
          monadCoreWorld.currentScene = scene
        case _ => ()
      gameLoop = GameLoop()

  override def initializeWorld(world: World): Unit =
    lock.synchronized:
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