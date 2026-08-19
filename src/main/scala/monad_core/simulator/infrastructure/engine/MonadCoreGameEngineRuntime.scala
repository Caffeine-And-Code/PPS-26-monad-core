package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.GameLoop
import monad_core.engine.model.{Entity, Scene, Vector2D}
import monad_core.engine.physics.core.PhysicsManager
import monad_core.engine.simulator.{EngineFacade, Painter}
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.{SaveEntityCommand, World}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.application.engine.errors.ErrorsAdapter.adaptError
import scalafx.animation.AnimationTimer

final class MonadCoreGameEngineRuntime(onError: BaseError => Unit = _ => ())
    extends GameEngineRuntime:
  private val lock                                        = new Object
  private var gameLoop                                    = GameLoop.default()
  private var currentWorld: Option[World]                 = None
  private var timer: Option[AnimationTimer]               = None
  private var currentSnapshot: Option[Scene]              = None
  private var error: Option[BaseError]                    = None
  private var currentDimensions: Option[(Double, Double)] = None

  given physics: PhysicsManager = PhysicsManager.default()

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
          val convertedState = world.scene

          EngineFacade.tick(gameLoop, convertedState, currentTime) match
            case Right((nextState, nextLoop)) =>
              (world, nextState) match
                case (monadCoreWorld: MonadCoreWorld, scene: Scene) =>
                  monadCoreWorld.currentScene = scene
                case _ => ()

              gameLoop = nextLoop
              renderer(world)

            case Left(engineError) =>
              val adaptedError = engineError.adaptError()
              error = Some(adaptedError)
              onError(adaptedError)
              stop()
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
      gameLoop = GameLoop.default()

  override def initializeWorld(
      world: World,
      withDefaultEntity: Boolean = true
  ): Either[BaseError, Unit] =
    val resizeResult = lock.synchronized:
      currentDimensions match
        case Some((width, height)) => world.resize(width, height)
        case None                  => Right(())

    resizeResult.flatMap { _ =>
      lock.synchronized:
        currentWorld = Some(world)

      if withDefaultEntity then
        Entity.circle(id = "starter", position = Vector2D(15, 15), radius = 15) match
          case Right(entity) =>
            world.createEntity(SaveEntityCommand(entity))
          case Left(error) =>
            Left(error.adaptError())
      else Right(())
    }

  override def getError: Option[BaseError] = error

  override def resize(width: Double, height: Double): Either[BaseError, Unit] =
    lock.synchronized:
      currentDimensions = Some((width, height))
      val resizeResult = currentWorld match
        case Some(world) => world.resize(width, height)
        case None        => Right(())

      resizeResult.foreach { _ =>
        currentSnapshot = currentSnapshot.flatMap { snapshot =>
          snapshot.resize(width, height).toOption
        }
      }

      resizeResult

object MonadCoreGameEngineRuntime:

  def apply(onError: BaseError => Unit = _ => ()): MonadCoreGameEngineRuntime =
    new MonadCoreGameEngineRuntime(onError)
