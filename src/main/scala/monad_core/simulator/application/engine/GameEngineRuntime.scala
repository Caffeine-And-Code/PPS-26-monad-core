package monad_core.simulator.application.engine

import monad_core.engine.simulator.{DrawCommand, Painter}
import monad_core.engine.simulator.EngineFacade.PhysicsRuleStatus
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.errors.BaseError

trait GameEngineRuntime extends EngineControl:
  def resize(width: Double, height: Double): Either[BaseError, Unit]
  def tick(currentTime: Long)(renderer: (World, Vector[DrawCommand]) => Unit)(using Painter): Unit
  def createSnapshot(): Unit
  def resetToSnapshot(): Unit

  def initializeWorld(
      world: World,
      withDefaultEntity: Boolean = true
  ): Either[BaseError, Unit]

  def getError: Option[BaseError]
  def physicsRules: Vector[PhysicsRuleStatus]
  def setPhysicsRuleEnabled(ruleId: String, isEnabled: Boolean): Unit
