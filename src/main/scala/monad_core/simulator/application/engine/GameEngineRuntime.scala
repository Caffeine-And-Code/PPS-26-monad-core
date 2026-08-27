package monad_core.simulator.application.engine

import monad_core.engine.simulator.{DrawCommand, Painter}
import monad_core.engine.simulator.EngineFacade.PhysicsRuleStatus
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.errors.BaseError

/** Application port coordinating an engine session, its world and frame rendering. */
trait GameEngineRuntime extends EngineControl:

  /**
   * Updates the dimensions used by the current and subsequently initialized worlds.
   *
   * @param width
   *   new world width
   * @param height
   *   new world height
   * @return
   *   `Right(())` on success, or the world validation error
   */
  def resize(width: Double, height: Double): Either[BaseError, Unit]

  /**
   * Advances the current world and delivers its drawing commands to a renderer.
   *
   * A contextual `Painter` supplies the strategy used to produce drawing commands.
   *
   * @param currentTime
   *   current monotonic time in nanoseconds
   * @param renderer
   *   consumer of the updated world and ordered drawing commands
   */
  def tick(currentTime: Long)(renderer: (World, Vector[DrawCommand]) => Unit)(using Painter): Unit

  /** Stores the current scene as the reset baseline. */
  def createSnapshot(): Unit

  /** Restores the stored scene and resets the engine session to edit mode. */
  def resetToSnapshot(): Unit

  /**
   * Installs the world controlled by this runtime.
   *
   * @param world
   *   world to initialize and retain
   * @param withDefaultEntity
   *   whether a starter entity must be inserted during initialization
   * @return
   *   `Right(())` on success, or the first resizing or entity-creation error
   */
  def initializeWorld(
      world: World,
      withDefaultEntity: Boolean = true
  ): Either[BaseError, Unit]

  /** @return the last engine error recorded by the runtime, when present */
  def getError: Option[BaseError]

  /** @return all configurable physics rules and their current enabled state */
  def physicsRules: Vector[PhysicsRuleStatus]

  /**
   * Enables or disables a physics rule.
   *
   * An unknown identifier leaves the runtime configuration unchanged.
   *
   * @param ruleId
   *   identifier of the target rule
   * @param isEnabled
   *   desired enabled state
   */
  def setPhysicsRuleEnabled(ruleId: String, isEnabled: Boolean): Unit
