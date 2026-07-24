package monad_core.engine.core

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{LocatableId, TeamId}

case class EntityNotFound(entityId: LocatableId) extends EngineError(s"Entity $entityId Not Found")

case class TeamNotFound(teamId: TeamId) extends EngineError(s"Team $teamId Not Found")

case class SurfaceNotFound(surfaceId: LocatableId) extends EngineError(s"Surface $surfaceId Not Found")

case class CannotAddEntity(genericError: CannotAddAlreadyPresentElementInMap[LocatableId])
  extends EngineError(s"Entity cannot be added to map.\n $genericError")

case class CannotAddTeam(genericError: CannotAddAlreadyPresentElementInMap[TeamId])
  extends EngineError(s"Team cannot be added to map.\n $genericError")

case class CannotAddSurface(genericError: CannotAddAlreadyPresentElementInMap[LocatableId])
  extends EngineError(s"Surface cannot be added to map.\n $genericError")

case class CannotAddAlreadyPresentElementInMap[Key](key: Key) 
  extends EngineError(s"Element with key $key already present of type ${key.getClass}")

case class CannotRemoveEntity(genericError: CannotRemoveNonPresentElementFromMap[LocatableId])
  extends EngineError(s"Entity cannot be removed from map.\n $genericError")

case class CannotRemoveTeam(genericError: CannotRemoveNonPresentElementFromMap[TeamId])
  extends EngineError(s"Team cannot be removed from map.\n $genericError")

case class CannotRemoveSurface(genericError: CannotRemoveNonPresentElementFromMap[LocatableId])
  extends EngineError(s"Surface cannot be removed from map.\n $genericError")

case class CannotRemoveNonPresentElementFromMap[Key](key:Key)
  extends EngineError(s"Element with key $key not present of type ${key.getClass}")