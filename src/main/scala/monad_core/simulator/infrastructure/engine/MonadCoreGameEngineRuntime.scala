package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.LoopMode
import monad_core.engine.core.events.EngineEvent
import monad_core.engine.model.{Entity, Scene, UnhandledStateType, Vector2D}
import monad_core.engine.physics.core.PhysicsManager
import monad_core.engine.simulator.{
  DrawCommand,
  EngineFacade,
  Painter,
  RendererManager,
  StateInterpolator
}
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

  private var physics: PhysicsManager = PhysicsManager.default()

  override def start(): Unit =
    lock.synchronized:
      currentWorld.foreach(_.enterSimulationMode())
      engineSession = EngineFacade.start(engineSession)

  override def stop(): Unit =
    lock.synchronized:
      engineSession = EngineFacade.stop(engineSession)
      currentWorld.foreach(_.enterEditMode())

  override def tick(currentTime: Long)(
      renderer: (World, Vector[DrawCommand]) => Unit
  )(using painter: Painter): Unit =
    lock.synchronized:
      currentWorld.foreach { world =>
        val currentScene = world.scene
        EngineFacade.tick(engineSession, currentScene, currentTime, physics) match
          case Right(tickResult) =>
            val processedFrame = for
              interpolatedScene <- StateInterpolator(
                previousScene = tickResult.previousState,
                nextScene = tickResult.state,
                interpolationAlpha = tickResult.alpha
              )
              commands <- RendererManager.render(interpolatedScene)
            yield (tickResult.state, commands)

            processedFrame match
              case Right((scene: Scene, commands)) =>
                world.replaceScene(scene)
                engineSession = tickResult.nextSession
                onEvents(tickResult.events)
                renderer(world, commands)

              case Left(engineError) =>
                handleError(engineError)

              case _ =>
                handleError(UnhandledStateType())

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

  override def createSnapshot(): Unit =
    lock.synchronized:
      currentSnapshot = currentWorld.map(_.scene)

  override def resetToSnapshot(): Unit =
    lock.synchronized:
      for
        world <- currentWorld
        scene <- currentSnapshot
      do world.replaceScene(scene)
      engineSession = EngineFacade.default
      currentWorld.foreach(_.enterEditMode())

  override def initializeWorld(
      world: World,
      withDefaultEntity: Boolean = true
  ): Either[BaseError, Unit] =
    lock.synchronized:
      for
        _ <- currentDimensions match
          case Some((width, height)) => world.resize(width, height)
          case None                  => Right(())
        _ <- addDefaultEntity(world, withDefaultEntity)
      yield
        synchronizeMode(world)
        currentWorld = Some(world)

  override def getError: Option[BaseError] = lock.synchronized(error)

  override def physicsRules: Vector[EngineFacade.PhysicsRuleStatus] =
    lock.synchronized(EngineFacade.physicsRules(physics))

  override def setPhysicsRuleEnabled(ruleId: String, isEnabled: Boolean): Unit =
    lock.synchronized:
      physics = EngineFacade.setPhysicsRuleEnabled(physics, ruleId, isEnabled)

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

  private def addDefaultEntity(
      world: World,
      withDefaultEntity: Boolean
  ): Either[BaseError, Unit] =
    if withDefaultEntity then
      Entity
        .circle(id = "starter", position = Vector2D(15, 15), radius = 15)
        .adaptError()
        .flatMap(entity => world.createEntity(SaveEntityCommand(entity)))
    else Right(())

  private def synchronizeMode(world: World): Unit =
    EngineFacade.mode(engineSession) match
      case LoopMode.EditMode       => world.enterEditMode()
      case LoopMode.SimulationMode => world.enterSimulationMode()

object MonadCoreGameEngineRuntime:

  def apply(
      onError: BaseError => Unit = _ => (),
      onEvents: Vector[EngineEvent] => Unit = _ => ()
  ): MonadCoreGameEngineRuntime =
    new MonadCoreGameEngineRuntime(onError, onEvents)
