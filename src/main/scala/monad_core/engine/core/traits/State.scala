package monad_core.engine.core.traits

import monad_core.engine.model.*

/**
 * Read and update contract required by the game loop, physics and rendering subsystems.
 *
 * Implementations expose world contents as immutable transitions: add and remove operations return a new state or an
 * explicit engine error.
 */
trait State:

  /** @return the current spatial boundaries of the world */
  def bounds: WorldBounds

  /** @return all entities in the state, with no ordering guarantee */
  def allEntities: List[Entity]

  /** @return all teams in the state, with no ordering guarantee */
  def allTeams: List[Team]

  /** @return all surfaces in the state, with no ordering guarantee */
  def allSurfaces: List[Surface]

  /**
   * Retrieves an entity by identifier.
   *
   * @param id
   *   entity identifier
   * @return
   *   the entity, or an engine error when it is absent
   */
  def getEntity(id: LocatableId): Either[EngineError, Entity]

  /**
   * Retrieves a team by identifier.
   *
   * @param id
   *   team identifier
   * @return
   *   the team, or an engine error when it is absent
   */
  def getTeam(id: TeamId): Either[EngineError, Team]

  /**
   * Retrieves a surface by identifier.
   *
   * @param id
   *   surface identifier
   * @return
   *   the surface, or an engine error when it is absent
   */
  def getSurface(id: LocatableId): Either[EngineError, Surface]

  /**
   * Adds an entity without mutating this state.
   *
   * @param entity
   *   entity to add
   * @return
   *   the updated state, or an engine error when its identifier is already present
   */
  def addEntity(entity: Entity): Either[EngineError, State]

  /**
   * Adds a team without mutating this state.
   *
   * @param team
   *   team to add
   * @return
   *   the updated state, or an engine error when its identifier is already present
   */
  def addTeam(team: Team): Either[EngineError, State]

  /**
   * Adds a surface without mutating this state.
   *
   * @param surface
   *   surface to add
   * @return
   *   the updated state, or an engine error when its identifier is already present
   */
  def addSurface(surface: Surface): Either[EngineError, State]

  /**
   * Removes an entity without mutating this state.
   *
   * @param entity
   *   entity whose identifier must be removed
   * @return
   *   the updated state, or an engine error when the identifier is absent
   */
  def removeEntity(entity: Entity): Either[EngineError, State]

  /**
   * Removes a team without mutating this state.
   *
   * @param team
   *   team whose identifier must be removed
   * @return
   *   the updated state, or an engine error when the identifier is absent
   */
  def removeTeam(team: Team): Either[EngineError, State]

  /**
   * Removes a surface without mutating this state.
   *
   * @param surface
   *   surface whose identifier must be removed
   * @return
   *   the updated state, or an engine error when the identifier is absent
   */
  def removeSurface(surface: Surface): Either[EngineError, State]
