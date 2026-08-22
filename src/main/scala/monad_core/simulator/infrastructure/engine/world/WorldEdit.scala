package monad_core.simulator.infrastructure.engine.world

import monad_core.engine.core.events.EngineEvent
import monad_core.engine.model.{Entity, LocatableId, Scene, Surface, Team, TeamId}
import monad_core.simulator.errors.BaseError

enum WorldEdit:
  case CreateEntity(entity: Entity)
  case UpdateEntity(entity: Entity)
  case RemoveEntity(id: LocatableId)
  case CreateSurface(surface: Surface)
  case UpdateSurface(surface: Surface)
  case RemoveSurface(id: LocatableId)
  case CreateTeam(team: Team)
  case UpdateTeam(team: Team)
  case RemoveTeam(id: TeamId)

final case class WorldEditResult(
    scene: Scene,
    events: Vector[EngineEvent] = Vector.empty
)

case object SceneEditingNotAllowed
    extends BaseError("The scene cannot be edited while the engine is running")
