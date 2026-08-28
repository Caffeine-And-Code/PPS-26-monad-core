package monad_core.engine.model

import monad_core.engine.core.*
import monad_core.engine.core.traits.State
import monad_core.engine.model.*
import monad_core.engine.model.Scene.*

/** Entities indexed by their locatable identifier. */
type EntityMap = Map[LocatableId, Entity]

/** Surfaces indexed by their locatable identifier. */
type SurfaceMap = Map[LocatableId, Surface]

/** Teams indexed by their team identifier. */
type TeamMap = Map[TeamId, Team]

/**
 * Minimal functional lens used to read and replace part of an immutable structure.
 *
 * @param get
 *   function that extracts the focused value
 * @param set
 *   function that returns a copy with a replacement focused value
 */
case class Lens[S, A](get: S => A, set: (S, A) => S)

/**
 * Immutable aggregate containing all objects in a simulation world.
 *
 * @param entities
 *   entities indexed by identifier
 * @param surfaces
 *   surfaces indexed by identifier
 * @param teams
 *   teams indexed by identifier
 * @param bounds
 *   spatial boundaries of the world
 */
case class Scene(
    entities: EntityMap = Map.empty,
    surfaces: SurfaceMap = Map.empty,
    teams: TeamMap = Map.empty,
    bounds: WorldBounds = WorldBounds.default
) extends State:

  /**
   * Retrieves an entity by identifier.
   *
   * @param id
   *   entity identifier
   * @return
   *   the entity, or `EntityNotFound` when absent
   */
  def getEntity(id: LocatableId): Either[EngineError, Entity] =
    getFromMap(Scene.entitiesLens, this, id, EntityNotFound(id))

  /**
   * Retrieves a team by identifier.
   *
   * @param id
   *   team identifier
   * @return
   *   the team, or `TeamNotFound` when absent
   */
  def getTeam(id: TeamId): Either[EngineError, Team] =
    getFromMap(Scene.teamsLens, this, id, TeamNotFound(id))

  /**
   * Retrieves a surface by identifier.
   *
   * @param id
   *   surface identifier
   * @return
   *   the surface, or `SurfaceNotFound` when absent
   */
  def getSurface(id: LocatableId): Either[EngineError, Surface] =
    getFromMap(Scene.surfacesLens, this, id, SurfaceNotFound(id))

  /**
   * Returns a scene containing the supplied entity.
   *
   * @param entity
   *   entity to add
   * @return
   *   the updated scene, or `CannotAddEntity` when its identifier already exists
   */
  def addEntity(entity: Entity): Either[EngineError, Scene] =
    addToMap(Scene.entitiesLens, this, entity.id, entity, CannotAddEntity(_))

  /**
   * Returns a scene containing the supplied team.
   *
   * @param team
   *   team to add
   * @return
   *   the updated scene, or `CannotAddTeam` when its identifier already exists
   */
  def addTeam(team: Team): Either[EngineError, Scene] =
    addToMap(Scene.teamsLens, this, team.id, team, CannotAddTeam(_))

  /**
   * Returns a scene containing the supplied surface.
   *
   * @param surface
   *   surface to add
   * @return
   *   the updated scene, or `CannotAddSurface` when its identifier already exists
   */
  def addSurface(surface: Surface): Either[EngineError, Scene] =
    addToMap(Scene.surfacesLens, this, surface.id, surface, CannotAddSurface(_))

  /**
   * Returns a scene without the supplied entity identifier.
   *
   * @param entity
   *   entity whose identifier must be removed
   * @return
   *   the updated scene, or `CannotRemoveEntity` when its identifier is absent
   */
  def removeEntity(entity: Entity): Either[EngineError, Scene] =
    removeFromMap(Scene.entitiesLens, this, entity.id, CannotRemoveEntity(_))

  /**
   * Returns a scene without the supplied team identifier.
   *
   * @param team
   *   team whose identifier must be removed
   * @return
   *   the updated scene, or `CannotRemoveTeam` when its identifier is absent
   */
  def removeTeam(team: Team): Either[EngineError, Scene] =
    removeFromMap(Scene.teamsLens, this, team.id, CannotRemoveTeam(_))

  /**
   * Returns a scene without the supplied surface identifier.
   *
   * @param surface
   *   surface whose identifier must be removed
   * @return
   *   the updated scene, or `CannotRemoveSurface` when its identifier is absent
   */
  def removeSurface(surface: Surface): Either[EngineError, Scene] =
    removeFromMap(Scene.surfacesLens, this, surface.id, CannotRemoveSurface(_))

  /** @return all entities in this scene, with no ordering guarantee */
  override def allEntities: List[Entity] = entities.map((id, entity) => entity).toList

  /** @return all teams in this scene, with no ordering guarantee */
  override def allTeams: List[Team] = teams.map((id, team) => team).toList

  /** @return all surfaces in this scene, with no ordering guarantee */
  override def allSurfaces: List[Surface] = surfaces.map((id, surfaces) => surfaces).toList

  /**
   * Returns a scene with updated spatial boundaries.
   *
   * @param width
   *   new world width
   * @param height
   *   new world height
   * @return
   *   the resized scene, or the validation error produced by `WorldBounds`
   */
  def resize(width: Double, height: Double): Either[EngineError, Scene] =
    val newBounds = WorldBounds(width, height)
    newBounds match
      case Left(err) => Left(err)
      case Right(bounds) =>
        Right(
          this.copy(
            bounds = bounds
          )
        )

/** Lenses and generic operations supporting immutable scene updates. */
object Scene:

  /** Lens focusing the entity map of a scene. */
  given entitiesLens: Lens[Scene, EntityMap] =
    Lens(_.entities, (s, m) => s.copy(entities = m))

  /** Lens focusing the team map of a scene. */
  given teamsLens: Lens[Scene, TeamMap] =
    Lens(_.teams, (s, m) => s.copy(teams = m))

  /** Lens focusing the surface map of a scene. */
  given surfacesLens: Lens[Scene, SurfaceMap] =
    Lens(_.surfaces, (s, m) => s.copy(surfaces = m))

  private def getFromMap[K, V](
      lens: Lens[Scene, Map[K, V]],
      scene: Scene,
      key: K,
      error: => EngineError
  ): Either[EngineError, V] =
    lens.get(scene).get(key).toRight(error)

  private def addToMap[K, V](
      lens: Lens[Scene, Map[K, V]],
      scene: Scene,
      key: K,
      value: V,
      error: CannotAddAlreadyPresentElementInMap[K] => EngineError
  ): Either[EngineError, Scene] =
    val m = lens.get(scene)
    if m.contains(key) then Left(error(CannotAddAlreadyPresentElementInMap(key)))
    else Right(lens.set(scene, m + (key -> value)))

  private def removeFromMap[K, V](
      lens: Lens[Scene, Map[K, V]],
      scene: Scene,
      key: K,
      error: CannotRemoveNonPresentElementFromMap[K] => EngineError
  ): Either[EngineError, Scene] =
    val m = lens.get(scene)
    if m.contains(key) then Right(lens.set(scene, m - key))
    else Left(error(CannotRemoveNonPresentElementFromMap(key)))
