package engine.core

import engine.core.traits.State
import engine.errors.EngineError
import engine.model.*

case class Scene(
                  entities: Map[LocatableId, Entity] = Map.empty,
                  surfaces: Map[LocatableId, Surface] = Map.empty,
                  teams: Map[TeamId, Team] = Map.empty
                ) extends State

object Scene {

  private def getFromMap[K, V](
                                map: Map[K, V],
                                key: K,
                                error: => EngineError
                              ): Either[EngineError, V] =
    map.get(key).toRight(error)

  private def addToMap[K, V](
                              map: Map[K, V],
                              key: K,
                              value: V
                            ): Either[CannotAddAlreadyPresentElementInMap[K], Map[K, V]] =
    if (map.contains(key))
      Left(CannotAddAlreadyPresentElementInMap(key))
    else
      Right(map + (key -> value))

  private def removeFromMap[K, V](
                                   map: Map[K, V],
                                   key: K,
                                 ): Either[CannotRemoveNonPresentElementFromMap[K], Map[K, V]] =
    if (map.contains(key))
      Right(map - key)
    else
      Left(CannotRemoveNonPresentElementFromMap(key))

  extension [E, A](either: Either[E, A])
    private def leftMap[F](f: E => F): Either[F, A] =
      either.left.map(f)

  extension (scene: Scene)

    // ENTITIES
    infix def getEntity(id: LocatableId): Either[EngineError, Entity] =
      getFromMap(scene.entities, id, EntityNotFound(id))

    infix def addEntity(toAdd: Entity): Either[EngineError, Scene] =
      addToMap(scene.entities, toAdd.id, toAdd)
        .leftMap(CannotAddEntity(_))
        .map(m => scene.copy(entities = m))

    infix def removeEntity(id: LocatableId): Either[EngineError, Scene] =
      removeFromMap(scene.entities, id)
        .leftMap(CannotRemoveEntity(_))
        .map(m => scene.copy(entities = m))

    // TEAMS
    infix def getTeam(id: TeamId): Either[EngineError, Team] =
      getFromMap(scene.teams, id, TeamNotFound(id))

    infix def addTeam(toAdd: Team): Either[EngineError, Scene] =
      addToMap(scene.teams, toAdd.id, toAdd)
        .leftMap(CannotAddTeam(_))
        .map(m => scene.copy(teams = m))

    infix def removeTeam(id: TeamId): Either[EngineError, Scene] =
      removeFromMap(scene.teams, id)
        .leftMap(CannotRemoveTeam(_))
        .map(m => scene.copy(teams = m))

    // SURFACES
    infix def getSurface(id: LocatableId): Either[EngineError, Surface] =
      getFromMap(scene.surfaces, id, SurfaceNotFound(id))

    infix def addSurface(toAdd: Surface): Either[EngineError, Scene] =
      addToMap(scene.surfaces, toAdd.id, toAdd)
        .leftMap(CannotAddSurface(_))
        .map(m => scene.copy(surfaces = m))

    infix def removeSurface(id: LocatableId): Either[EngineError, Scene] =
      removeFromMap(scene.surfaces, id)
        .leftMap(CannotRemoveSurface(_))
        .map(m => scene.copy(surfaces = m))
}