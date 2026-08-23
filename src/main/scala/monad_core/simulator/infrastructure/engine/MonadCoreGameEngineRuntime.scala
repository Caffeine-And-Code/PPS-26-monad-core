package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.LoopMode
import monad_core.engine.core.events.EngineEvent
import monad_core.engine.model.{Entity, Scene, Vector2D}
import monad_core.engine.physics.core.PhysicsManager
import monad_core.engine.simulator.{EngineFacade, Painter, RendererManager, SceneInterpolator}
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.errors.ErrorsAdapter.adaptError
import monad_core.simulator.application.engine.world.{SaveEntityCommand, World}
import monad_core.simulator.errors.BaseError

final class MonadCoreGameEngineRuntime(
    onError: BaseError => Unit = _ => (),
    onEvents: Vector[EngineEvent] => Unit = _ => ()
) extends GameEngineRuntime:
  private val lock                                        = new Object
  private var engineSession                               = EngineFacade.default
  private var currentWorld: Option[World]                 = None
  private var currentSnapshot: Option[Scene]              = None
  private var error: Option[BaseError]                    = None
  private var currentDimensions: Option[(Double, Double)] = None

  given physics: PhysicsManager = PhysicsManager.default()

  override def start(): Unit =
    lock.synchronized:
      engineSession = EngineFacade.start(engineSession)

  override def stop(): Unit =
    lock.synchronized:
      engineSession = EngineFacade.stop(engineSession)

  override def tick(currentTime: Long)(renderer: World => Unit)(using painter: Painter): Unit =
    lock.synchronized:
      currentWorld.foreach { world =>
        EngineFacade.tick(engineSession, world.scene, currentTime, physics) match
          case Right(tickResult) =>
            tickResult.state match
              case scene: Scene =>
                val processedScene = for
                  interpolatedScene <- SceneInterpolator(
                    previousScene = world.scene,
                    nextScene = scene,
                    interpolationAlpha = tickResult.alpha
                  )
                  _ <- RendererManager.render(interpolatedScene)
                yield scene

                processedScene match
                  case Right(scene) =>
                    world match
                      case monadCoreWorld: MonadCoreWorld =>
                        monadCoreWorld.currentScene = scene
                      case _ => ()

                    engineSession = tickResult.nextSession
                    onEvents(tickResult.events)
                    renderer(world)

                  case Left(engineError) =>
                    handleError(engineError)

              case _ => ()

          case Left(engineError) =>
            handleError(engineError)
      }

  private def handleError(engineError: monad_core.engine.model.EngineError): Unit =
    val adaptedError = engineError.adaptError()
    error = Some(adaptedError)
    onError(adaptedError)
    stop()

  override def isRunning: Boolean =
    lock.synchronized(EngineFacade.isRunning(engineSession))

  def mode: LoopMode =
    lock.synchronized(EngineFacade.mode(engineSession))

  override def createSnapshot(): Unit =
    lock.synchronized:
      currentSnapshot = currentWorld.map(_.scene)

  override def resetToSnapshot(): Unit =
    lock.synchronized:
      (currentWorld, currentSnapshot) match
        case (Some(monadCoreWorld: MonadCoreWorld), Some(scene)) =>
          monadCoreWorld.currentScene = scene
        case _ => ()
      engineSession = EngineFacade.default

  override def initializeWorld(
      world: World,
      withDefaultEntity: Boolean = true
  ): Either[BaseError, Unit] =
    val setCurrentWorld: () => Unit = () =>
      lock.synchronized:
        currentWorld = Some(world)

    val resizeResult = lock.synchronized:
      currentDimensions match
        case Some((width, height)) => world.resize(width, height)
        case None                  => Right(())

    resizeResult.flatMap { _ =>
      if withDefaultEntity then
        Entity.circle(id = "starter", position = Vector2D(15, 15), radius = 15) match
          case Right(entity) =>
            world.createEntity(
              SaveEntityCommand(entity)
            )

            setCurrentWorld()

            Right(())
          case Left(error) => Left(error.adaptError())
      else {
        setCurrentWorld()
        Right(())
      }
    }

  override def getError: Option[BaseError] = lock.synchronized(error)

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

  def apply(
      onError: BaseError => Unit = _ => (),
      onEvents: Vector[EngineEvent] => Unit = _ => ()
  ): MonadCoreGameEngineRuntime =
    new MonadCoreGameEngineRuntime(onError, onEvents)
