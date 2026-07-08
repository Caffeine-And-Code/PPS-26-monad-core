package engine.core

import engine.core.traits.State
import engine.errors.*
import engine.model.*

type EntityMap = Map[LocatableId, Entity]
type SurfaceMap = Map[LocatableId, Surface]
type TeamMap = Map[TeamId, Team]

case class Scene(
                  entities: EntityMap = Map.empty,
                  surfaces: SurfaceMap = Map.empty,
                  teams: TeamMap = Map.empty
                ):

  private val ops = summon[State[Scene]]

  def getEntity(id: LocatableId): Either[EngineError, Entity] = ops.getEntity(this, id)

  def getTeam(id: TeamId): Either[EngineError, Team] = ops.getTeam(this, id)

  def getSurface(id: LocatableId): Either[EngineError, Surface] = ops.getSurface(this, id)

  def addEntity(entity: Entity): Either[EngineError, Scene] = ops.addEntity(this, entity)

  def addTeam(team: Team): Either[EngineError, Scene] = ops.addTeam(this, team)

  def addSurface(surface: Surface): Either[EngineError, Scene] = ops.addSurface(this, surface)

  def removeEntity(entity: Entity): Either[EngineError, Scene] = ops.removeEntity(this, entity)

  def removeTeam(team: Team): Either[EngineError, Scene] = ops.removeTeam(this, team)

  def removeSurface(surface: Surface): Either[EngineError, Scene] = ops.removeSurface(this, surface)

  def updateEntity(entity: Entity): Either[EngineError, Scene] = ops.updateEntity(this, entity)

  def updateTeam(team: Team): Either[EngineError, Scene] = ops.updateTeam(this, team)

  def updateSurface(surface: Surface): Either[EngineError, Scene] = ops.updateSurface(this, surface)

object Scene:

  private val entitiesLens: Lens[Scene, EntityMap] =
    Lens(_.entities, (s, m) => s.copy(entities = m))

  private val teamsLens: Lens[Scene, TeamMap] =
    Lens(_.teams, (s, m) => s.copy(teams = m))

  private val surfacesLens: Lens[Scene, SurfaceMap] =
    Lens(_.surfaces, (s, m) => s.copy(surfaces = m))

  private def getFromMap[K, V](
                                lens: Lens[Scene, Map[K, V]],
                                scene: Scene,
                                key: K
                              )(error: => EngineError): Either[EngineError, V] =
    lens.get(scene).get(key).toRight(error)

  private def addToMap[K, V](
                              lens: Lens[Scene, Map[K, V]],
                              scene: Scene,
                              key: K,
                              value: V
                            )(error: CannotAddAlreadyPresentElementInMap[K] => EngineError): Either[EngineError, Scene] =
    val m = lens.get(scene)
    if m.contains(key) then Left(error(CannotAddAlreadyPresentElementInMap(key)))
    else Right(lens.set(scene, m + (key -> value)))

  private def removeFromMap[K, V](
                                   lens: Lens[Scene, Map[K, V]],
                                   scene: Scene,
                                   key: K
                                 )(error: CannotRemoveNonPresentElementFromMap[K] => EngineError): Either[EngineError, Scene] =
    val m = lens.get(scene)
    if m.contains(key) then Right(lens.set(scene, m - key))
    else Left(error(CannotRemoveNonPresentElementFromMap(key)))

  private def updateInMap[K, V](
                                 lens: Lens[Scene, Map[K, V]],
                                 scene: Scene,
                                 key: K,
                                 newValue: V,
                               )(notFoundError: K => EngineError): Either[EngineError, Scene] =
    val m = lens.get(scene)
    if m.contains(key) then Right(lens.set(scene, m.updated(key, newValue)))
    else Left(notFoundError(key))

  given State[Scene] with

    def getEntity(scene: Scene, id: LocatableId): Either[EngineError, Entity] =
      getFromMap(entitiesLens, scene, id)(EntityNotFound(id))

    def getTeam(scene: Scene, id: TeamId): Either[EngineError, Team] =
      getFromMap(teamsLens, scene, id)(TeamNotFound(id))

    def getSurface(scene: Scene, id: LocatableId): Either[EngineError, Surface] =
      getFromMap(surfacesLens, scene, id)(SurfaceNotFound(id))

    def addEntity(scene: Scene, entity: Entity): Either[EngineError, Scene] =
      addToMap(entitiesLens, scene, entity.id, entity)(CannotAddEntity(_))

    def addTeam(scene: Scene, team: Team): Either[EngineError, Scene] =
      addToMap(teamsLens, scene, team.id, team)(CannotAddTeam(_))

    def addSurface(scene: Scene, surface: Surface): Either[EngineError, Scene] =
      addToMap(surfacesLens, scene, surface.id, surface)(CannotAddSurface(_))

    def removeEntity(scene: Scene, entity: Entity): Either[EngineError, Scene] =
      removeFromMap(entitiesLens, scene, entity.id)(CannotRemoveEntity(_))

    def removeTeam(scene: Scene, team: Team): Either[EngineError, Scene] =
      removeFromMap(teamsLens, scene, team.id)(CannotRemoveTeam(_))

    def removeSurface(scene: Scene, surface: Surface): Either[EngineError, Scene] =
      removeFromMap(surfacesLens, scene, surface.id)(CannotRemoveSurface(_))

    def updateEntity(scene: Scene, updatedEntity: Entity): Either[EngineError, Scene] =
      updateInMap(entitiesLens, scene, updatedEntity.id, updatedEntity)(EntityNotFound(_))

    def updateTeam(scene: Scene, updatedTeam: Team): Either[EngineError, Scene] =
      updateInMap(teamsLens, scene, updatedTeam.id, updatedTeam)(TeamNotFound(_))

    def updateSurface(scene: Scene, updatedSurface: Surface): Either[EngineError, Scene] =
      updateInMap(surfacesLens, scene, updatedSurface.id, updatedSurface)(SurfaceNotFound(_))