package engine.core

import engine.core.Scene.{entitiesLens, surfacesLens, teamsLens, *}
import engine.core.traits.State
import engine.errors.EngineError
import engine.model.*

type EntityMap = Map[LocatableId, Entity]
type SurfaceMap = Map[LocatableId, Surface]
type TeamMap = Map[TeamId, Team]

case class Lens[S, A](get: S => A, set: (S, A) => S)

case class Scene(
                  entities: EntityMap = Map.empty,
                  surfaces: SurfaceMap = Map.empty,
                  teams: TeamMap = Map.empty
                ) extends State:
  // Gets
  def getEntity(id: LocatableId): Either[EngineError, Entity] =
    getFromMap(entitiesLens, this, id, EntityNotFound(id))

  def getTeam(id: TeamId): Either[EngineError, Team] =
    getFromMap(teamsLens, this, id, TeamNotFound(id))

  def getSurface(id: LocatableId): Either[EngineError, Surface] =
    getFromMap(surfacesLens, this, id, SurfaceNotFound(id))

  // Adds
  def addEntity(entity: Entity): Either[EngineError, Scene] =
    addToMap(entitiesLens, this, entity.id, entity, CannotAddEntity(_))

  def addTeam(team: Team): Either[EngineError, Scene] =
    addToMap(teamsLens, this, team.id, team, CannotAddTeam(_))

  def addSurface(surface: Surface): Either[EngineError, Scene] =
    addToMap(surfacesLens, this, surface.id, surface, CannotAddSurface(_))

  // Removes
  def removeEntity(entity: Entity): Either[EngineError, Scene] =
    removeFromMap(entitiesLens, this, entity.id, CannotRemoveEntity(_))

  def removeTeam(team: Team): Either[EngineError, Scene] =
    removeFromMap(teamsLens, this, team.id, CannotRemoveTeam(_))

  def removeSurface(surface: Surface): Either[EngineError, Scene] =
    removeFromMap(surfacesLens, this, surface.id, CannotRemoveSurface(_))

object Scene:

  given entitiesLens: Lens[Scene, EntityMap] =
    Lens(_.entities, (s, m) => s.copy(entities = m))

  given teamsLens: Lens[Scene, TeamMap] =
    Lens(_.teams, (s, m) => s.copy(teams = m))

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