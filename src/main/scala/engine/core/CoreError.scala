package engine.core

import engine.errors.EngineError
import engine.model.{LocatableId, TeamId}

case class EntityNotFound(entityId: LocatableId) extends EngineError(s"Entity $entityId Not Found")

case class TeamNotFound(teamId: TeamId) extends EngineError(s"Team $teamId Not Found")

case class SurfaceNotFound(surfaceId: LocatableId) extends EngineError(s"Surface $surfaceId Not Found")

case class CannotAddAlreadyPresentElementInMap[Key](key: Key) extends EngineError(s"Element with key $key already present of type ${key.getClass}")

case class CannotRemoveNonPresentElementFromMap[Key](key:Key) extends EngineError(s"Element with key $key not present of type ${key.getClass}")