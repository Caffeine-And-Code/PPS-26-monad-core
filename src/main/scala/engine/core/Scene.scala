package engine.core

import engine.errors.EngineError
import engine.model.*

type EntityMap = Map[LocatableId, Entity]
type SurfaceMap = Map[LocatableId, Surface]
type TeamMap = Map[TeamId, Team]

private case class Lens[S, A](get: S => A, set: (S, A) => S)

case class Scene(
                  entities: EntityMap = Map.empty,
                  surfaces: SurfaceMap = Map.empty,
                  teams: TeamMap = Map.empty
                )

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
                              error: K => EngineError
                            ): Either[EngineError, Scene] =
    val m = lens.get(scene)
    if m.contains(key) then Left(error(key))
    else Right(lens.set(scene, m + (key -> value)))

  private def removeFromMap[K, V](
                                   lens: Lens[Scene, Map[K, V]],
                                   scene: Scene,
                                   key: K,
                                   error: K => EngineError
                                 ): Either[EngineError, Scene] =
    val m = lens.get(scene)
    if m.contains(key) then Right(lens.set(scene, m - key))
    else Left(error(key))

  private def updateInMap[K, V](
                                 lens: Lens[Scene, Map[K, V]],
                                 scene: Scene,
                                 key: K,
                                 newValue: V,
                                 notFoundError: K => EngineError
                               ): Either[EngineError, Scene] =
    val m = lens.get(scene)
    if m.contains(key) then Right(lens.set(scene, m.updated(key, newValue)))
    else Left(notFoundError(key))

  extension (scene: Scene)

    // Gets
    def getEntity(id: LocatableId): Either[EngineError, Entity] =
      getFromMap(entitiesLens, scene, id, EntityNotFound(id))

    def getTeam(id: TeamId): Either[EngineError, Team] =
      getFromMap(teamsLens, scene, id, TeamNotFound(id))

    def getSurface(id: LocatableId): Either[EngineError, Surface] =
      getFromMap(surfacesLens, scene, id, SurfaceNotFound(id))

    // Adds
    def addEntity(entity: Entity): Either[EngineError, Scene] =
      addToMap(entitiesLens, scene, entity.id, entity,
        k => CannotAddEntity(CannotAddAlreadyPresentElementInMap(k)))

    def addTeam(team: Team): Either[EngineError, Scene] =
      addToMap(teamsLens, scene, team.id, team,
        k => CannotAddTeam(CannotAddAlreadyPresentElementInMap(k)))

    def addSurface(surface: Surface): Either[EngineError, Scene] =
      addToMap(surfacesLens, scene, surface.id, surface,
        k => CannotAddSurface(CannotAddAlreadyPresentElementInMap(k)))

    // Removes
    def removeEntity(entity: Entity): Either[EngineError, Scene] =
      removeFromMap(entitiesLens, scene, entity.id,
        k => CannotRemoveEntity(CannotRemoveNonPresentElementFromMap(k)))

    def removeTeam(team: Team): Either[EngineError, Scene] =
      removeFromMap(teamsLens, scene, team.id,
        k => CannotRemoveTeam(CannotRemoveNonPresentElementFromMap(k)))

    def removeSurface(surface: Surface): Either[EngineError, Scene] =
      removeFromMap(surfacesLens, scene, surface.id,
        k => CannotRemoveSurface(CannotRemoveNonPresentElementFromMap(k)))

    //Updates
    def updateEntity(updatedEntity: Entity): Either[EngineError, Scene] =
      updateInMap(entitiesLens, scene, updatedEntity.id, updatedEntity, EntityNotFound(_))

    def updateTeam(updatedTeam: Team): Either[EngineError, Scene] =
      updateInMap(teamsLens, scene, updatedTeam.id, updatedTeam, TeamNotFound(_))

    def updateSurface(updatedSurface: Surface): Either[EngineError, Scene] =
      updateInMap(surfacesLens, scene, updatedSurface.id, updatedSurface, SurfaceNotFound(_))