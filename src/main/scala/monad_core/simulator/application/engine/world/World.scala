package monad_core.simulator.application.engine.world

import monad_core.engine.model.Scene
import monad_core.simulator.errors.BaseError

/**
 * Application port exposing the current engine scene and its supported mutations.
 *
 * Entity, surface and team operations are inherited from their dedicated contracts. Implementations also coordinate
 * the scene dimensions and the editing mode enforced by the underlying world adapter.
 */
trait World extends TeamOperations, EntityOperations, SurfaceOperations:

  /** @return the scene currently held by the world */
  def scene: Scene

  /**
   * Updates the spatial dimensions of the world.
   *
   * @param width
   *   new world width
   * @param height
   *   new world height
   * @return
   *   `Right(())` on success, or a validation error when the dimensions are invalid
   */
  def resize(width: Double, height: Double): Either[BaseError, Unit]

  /**
   * Replaces the complete scene held by the world.
   *
   * This operation is intended for engine-controlled state transitions rather than user editing commands.
   *
   * @param scene
   *   scene that becomes current
   */
  def replaceScene(scene: Scene): Unit

  /** Allows world editing operations. */
  def enterEditMode(): Unit

  /** Rejects world editing operations while simulation is active. */
  def enterSimulationMode(): Unit
