package monad_core.simulator.application.engine.world

import monad_core.engine.model.Entity
import monad_core.simulator.errors.BaseError

/**
 * Application command carrying an entity to be created or updated.
 *
 * @param entity entity supplied to the world operation
 */
case class SaveEntityCommand(
    entity: Entity
)

/**
 * Contract for querying and editing the entities contained in a world.
 *
 * Mutation errors from the engine domain are exposed as [[BaseError]], and mutations
 * will be rejected while the world is in simulation mode.
 */
private[world] trait EntityOperations:

  /** @return all entities currently contained in the world, with no ordering guarantee */
  def getAllEntities: List[Entity]

  /**
   * Retrieves an entity from its external string identifier.
   *
   * @param entityId raw entity identifier
   * @return the matching entity, or a validation/not-found error
   */
  def getEntity(entityId: String): Either[BaseError, Entity]

  /**
   * Adds an entity to the world.
   *
   * @param command command containing the entity to add
   * @return `Right(Unit)` on success, or `Left(BaseError)` when the edit is invalid or not allowed
   */
  def createEntity(command: SaveEntityCommand): Either[BaseError, Unit]

  /**
   * Removes an entity using its external string identifier.
   *
   * @param entityId raw identifier of the entity to remove
   * @return `Right(Unit)` on success, or `Left(BaseError)` when the identifier is invalid,
   *         the entity is missing, or the edit is not allowed
   */
  def removeEntity(entityId: String): Either[BaseError, Unit]

  /**
   * Replaces the entity having the same identifier as the command entity.
   *
   * @param command command containing the updated entity
   * @return `Right(Unit)` on success, or `Left(BaseError)` when the entity is missing,
   *         the update is invalid, or the edit is not allowed
   */
  def updateEntity(command: SaveEntityCommand): Either[BaseError, Unit]
