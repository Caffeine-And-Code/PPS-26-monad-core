package monad_core.simulator.infrastructure.engine.world

import monad_core.engine.core.events.EngineEvent
import monad_core.engine.model.{Entity, LocatableId, Scene, Surface, Team, TeamId}
import monad_core.simulator.errors.BaseError

/**
 * Immutable command describing a single mutation of an engine
 * [[monad_core.engine.model.Scene Scene]].
 *
 * Commands are interpreted by [[WorldEditor]] and do not mutate a scene directly.
 */
enum WorldEdit:

  /** Adds an entity to the scene. */
  case CreateEntity(entity: Entity)

  /** Replaces the entity with the same identifier. */
  case UpdateEntity(entity: Entity)

  /** Removes an entity by identifier. */
  case RemoveEntity(id: LocatableId)

  /** Adds a surface to the scene. */
  case CreateSurface(surface: Surface)

  /** Replaces the surface with the same identifier. */
  case UpdateSurface(surface: Surface)

  /** Removes a surface by identifier. */
  case RemoveSurface(id: LocatableId)

  /** Adds a team to the scene. */
  case CreateTeam(team: Team)

  /** Replaces the team with the same identifier. */
  case UpdateTeam(team: Team)

  /** Removes a team by identifier. */
  case RemoveTeam(id: TeamId)

/**
 * Result of a successfully interpreted [[WorldEdit]].
 *
 * Entity edits produce lifecycle events; surface and team edits currently return an empty
 * event collection.
 *
 * @param scene updated immutable scene
 * @param events events emitted by the edit
 */
final case class WorldEditResult(
    scene: Scene,
    events: Vector[EngineEvent] = Vector.empty
)

/** Error returned when a world mutation is requested while the engine is in simulation mode. */
case object SceneEditingNotAllowed
    extends BaseError("The scene cannot be edited while the engine is running")
