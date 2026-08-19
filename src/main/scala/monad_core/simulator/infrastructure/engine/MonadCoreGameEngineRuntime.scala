package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.GameLoop
import monad_core.engine.model.{Entity, Scene, Vector2D}
import monad_core.engine.physics.core.PhysicsManager
import monad_core.engine.simulator.{EngineFacade, Painter}
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.errors.ErrorsAdapter.adaptError
import monad_core.simulator.application.engine.world.{SaveEntityCommand, World}
import monad_core.simulator.errors.BaseError

final class MonadCoreGameEngineRuntime extends GameEngineRuntime:
  private val lock                           = new Object
  private var gameLoop                       = GameLoop.default()
  private var currentWorld: Option[World]    = None
  private var currentSnapshot: Option[Scene] = None
  private var error: Option[BaseError]       = None

  given physics: PhysicsManager = PhysicsManager.default()

  override def start(): Unit =
    lock.synchronized:
      gameLoop = gameLoop.start()

  override def stop(): Unit =
    lock.synchronized:
      gameLoop = gameLoop.stop()

  override def tick(currentTime: Long)(renderer: World => Unit)(using painter: Painter): Unit =
    lock.synchronized:
      currentWorld.foreach { world =>
        EngineFacade.tick(gameLoop, world.scene, currentTime) match
          case Right((nextState, nextLoop)) =>
            (world, nextState) match
              case (monadCoreWorld: MonadCoreWorld, scene: Scene) =>
                monadCoreWorld.currentScene = world.scene
              case _ => ()

            gameLoop = nextLoop
            renderer(world)

          case Left(engineError) =>
            error = Some(engineError.adaptError())
            stop()
      }

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
    val setCurrentWorld: () => Unit = () =>
      lock.synchronized:
        currentWorld = Some(world)

    if withDefaultEntity then
      Entity.circle(id = "starter", position = Vector2D(15, 15), radius = 15) match
        case Right(entity) =>
          world.createEntity(
            SaveEntityCommand(entity)
          )

          setCurrentWorld()

          Right(())
        case Left(error) => Left(error.adaptError())
    else
      setCurrentWorld()
      Right(())

  override def getError: Option[BaseError] = error

object MonadCoreGameEngineRuntime:
  def apply(): MonadCoreGameEngineRuntime = new MonadCoreGameEngineRuntime
