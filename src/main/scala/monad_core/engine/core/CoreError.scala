package monad_core.engine.core

import monad_core.engine.model.{EngineError, LocatableId, TeamId}

/**
 * Indicates that an entity identifier is absent from a scene.
 *
 * @param entityId
 *   identifier requested by the caller
 */
case class EntityNotFound(entityId: LocatableId) extends EngineError(s"Entity $entityId Not Found")

/**
 * Indicates that a team identifier is absent from a scene.
 *
 * @param teamId
 *   identifier requested by the caller
 */
case class TeamNotFound(teamId: TeamId) extends EngineError(s"Team $teamId Not Found")

/**
 * Indicates that a surface identifier is absent from a scene.
 *
 * @param surfaceId
 *   identifier requested by the caller
 */
case class SurfaceNotFound(surfaceId: LocatableId)
    extends EngineError(s"Surface $surfaceId Not Found")

/**
 * Wraps a duplicate-key failure raised while adding an entity.
 *
 * @param genericError
 *   underlying map error containing the duplicate identifier
 */
case class CannotAddEntity(genericError: CannotAddAlreadyPresentElementInMap[LocatableId])
    extends EngineError(s"Entity cannot be added to map.\n $genericError")

/**
 * Wraps a duplicate-key failure raised while adding a team.
 *
 * @param genericError
 *   underlying map error containing the duplicate identifier
 */
case class CannotAddTeam(genericError: CannotAddAlreadyPresentElementInMap[TeamId])
    extends EngineError(s"Team cannot be added to map.\n $genericError")

/**
 * Wraps a duplicate-key failure raised while adding a surface.
 *
 * @param genericError
 *   underlying map error containing the duplicate identifier
 */
case class CannotAddSurface(genericError: CannotAddAlreadyPresentElementInMap[LocatableId])
    extends EngineError(s"Surface cannot be added to map.\n $genericError")

/**
 * Indicates that a map insertion would overwrite an existing key.
 *
 * @param key
 *   duplicate key
 */
case class CannotAddAlreadyPresentElementInMap[Key](key: Key)
    extends EngineError(s"Element with key $key already present of type ${key.getClass}")

/**
 * Wraps a missing-key failure raised while removing an entity.
 *
 * @param genericError
 *   underlying map error containing the absent identifier
 */
case class CannotRemoveEntity(genericError: CannotRemoveNonPresentElementFromMap[LocatableId])
    extends EngineError(s"Entity cannot be removed from map.\n $genericError")

/**
 * Wraps a missing-key failure raised while removing a team.
 *
 * @param genericError
 *   underlying map error containing the absent identifier
 */
case class CannotRemoveTeam(genericError: CannotRemoveNonPresentElementFromMap[TeamId])
    extends EngineError(s"Team cannot be removed from map.\n $genericError")

/**
 * Wraps a missing-key failure raised while removing a surface.
 *
 * @param genericError
 *   underlying map error containing the absent identifier
 */
case class CannotRemoveSurface(genericError: CannotRemoveNonPresentElementFromMap[LocatableId])
    extends EngineError(s"Surface cannot be removed from map.\n $genericError")

/**
 * Indicates that a map removal targets an absent key.
 *
 * @param key
 *   key that could not be found
 */
case class CannotRemoveNonPresentElementFromMap[Key](key: Key)
    extends EngineError(s"Element with key $key not present of type ${key.getClass}")

/**
 * Indicates that an interpolation coefficient lies outside the inclusive range `[0, 1]`.
 *
 * @param alpha
 *   invalid coefficient
 */
case class InvalidInterpolationAlpha(alpha: Double)
    extends EngineError(s"Interpolation alpha must be between 0 and 1: $alpha")
