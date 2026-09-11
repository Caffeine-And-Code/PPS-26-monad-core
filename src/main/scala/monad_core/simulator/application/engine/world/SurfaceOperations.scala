package monad_core.simulator.application.engine.world

import monad_core.engine.model.Surface
import monad_core.simulator.errors.BaseError

/**
 * Application command carrying a surface to be created or updated.
 *
 * @param surface surface supplied to the world operation
 */
case class SaveSurfaceCommand(
    surface: Surface
)

/**
 * Contract for querying and editing the surfaces contained in a world.
 *
 * Mutation errors from the engine domain are exposed as [[BaseError]], and mutations
 * will be rejected while the world is in simulation mode.
 */
private[world] trait SurfaceOperations:

  /** @return all surfaces currently contained in the world, with no ordering guarantee */
  def getAllSurfaces: List[Surface]

  /**
   * Retrieves a surface from its external string identifier.
   *
   * @param id raw surface identifier
   * @return the matching surface, or a validation/not-found error
   */
  def getSurface(id: String): Either[BaseError, Surface]

  /**
   * Adds a surface to the world.
   *
   * @param command command containing the surface to add
   * @return `Right(Unit)` on success, or `Left(BaseError)` when the edit is invalid or not allowed
   */
  def createSurface(command: SaveSurfaceCommand): Either[BaseError, Unit]

  /**
   * Removes a surface using its external string identifier.
   *
   * @param id raw identifier of the surface to remove
   * @return `Right(Unit)` on success, or `Left(BaseError)` when the identifier is invalid,
   *         the surface is missing, or the edit is not allowed
   */
  def removeSurface(id: String): Either[BaseError, Unit]

  /**
   * Replaces the surface having the same identifier as the command surface.
   *
   * @param command command containing the updated surface
   * @return `Right(Unit)` on success, or `Left(BaseError)` when the surface is missing,
   *         the update is invalid, or the edit is not allowed
   */
  def updateSurface(command: SaveSurfaceCommand): Either[BaseError, Unit]
