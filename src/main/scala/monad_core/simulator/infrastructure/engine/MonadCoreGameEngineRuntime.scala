package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.{GameLoop, RendererManager}
import monad_core.engine.model.{Entity, Scene, Vector2D}
import monad_core.engine.physics.core.PhysicsManager
import monad_core.engine.simulator.{EngineFacade, EventDispatcher, EventManager, Painter, dispatchEvents, registerEvents}
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
          case Right(tickResult) =>
            tickResult.state match
              case _: Scene =>
                val eventManager = EventManager().registerEvents(tickResult.events)
                val (dispatchedScene, _) =
                  eventManager.dispatchEvents(world.scene)(EventDispatcher.handle)

                val processedScene = for
                  scene <- dispatchedScene
                  _     <- RendererManager.render(scene, tickResult.alpha)
                yield scene

                processedScene match
                  case Right(scene) =>
                    world match
                      case monadCoreWorld: MonadCoreWorld =>
                        monadCoreWorld.currentScene = scene
                      case _ => ()

                    gameLoop = tickResult.loop
                    renderer(world)

                  case Left(engineError) =>
                    handleError(engineError)

              case _ => ()

          case Left(engineError) =>
            handleError(engineError)
      }

  private def handleError(engineError: monad_core.engine.model.EngineError): Unit =
    error = Some(engineError.adaptError())
    gameLoop = gameLoop.stop()

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

    val initializedWorld =
      if withDefaultEntity then
        for
          entity <- Entity
            .circle(id = "starter", position = Vector2D(15, 15), radius = 15)
            .adaptError()
          _ <- world.createEntity(SaveEntityCommand(entity))
        yield ()
      else Right(())

    initializedWorld.map { _ =>
      setCurrentWorld()
      ()
    }

  override def getError: Option[BaseError] =
    lock.synchronized(error)

object MonadCoreGameEngineRuntime:
  def apply(): MonadCoreGameEngineRuntime = new MonadCoreGameEngineRuntime
