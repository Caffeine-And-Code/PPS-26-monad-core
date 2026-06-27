package engine.core

import engine.core.traits.State
import engine.errors.EngineError
import engine.model.*

type EntityMap = Map[LocatableId, Entity]
type SurfaceMap = Map[LocatableId, Surface]
type TeamMap = Map[TeamId, Team]

case class Scene(
                  entities: EntityMap = Map.empty,
                  surfaces: SurfaceMap = Map.empty,
                  teams: TeamMap = Map.empty
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

    // Gets
    def getEntity(id: LocatableId): Either[EngineError, Entity] =
      getFromMap(scene.entities, id, EntityNotFound(id))

    def getTeam(id: TeamId): Either[EngineError, Team] =
      getFromMap(scene.teams, id, TeamNotFound(id))

    def getSurface(id: LocatableId): Either[EngineError, Surface] =
      getFromMap(scene.surfaces, id, SurfaceNotFound(id))

    // Adds
    def +(toAdd: Entity): Either[EngineError, Scene] =
      addToMap(scene.entities, toAdd.id, toAdd)
        .leftMap(CannotAddEntity(_))
        .map(m => scene.copy(entities = m))

    def +(toAdd: Team): Either[EngineError, Scene] =
      addToMap(scene.teams, toAdd.id, toAdd)
        .leftMap(CannotAddTeam(_))
        .map(m => scene.copy(teams = m))

    def +(toAdd: Surface): Either[EngineError, Scene] =
      addToMap(scene.surfaces, toAdd.id, toAdd)
        .leftMap(CannotAddSurface(_))
        .map(m => scene.copy(surfaces = m))

    // Removes
    def -(entity: Entity): Either[EngineError, Scene] =
      removeFromMap(scene.entities, entity.id)
        .leftMap(CannotRemoveEntity(_))
        .map(m => scene.copy(entities = m))

    def -(team: Team): Either[EngineError, Scene] =
      removeFromMap(scene.teams, team.id)
        .leftMap(CannotRemoveTeam(_))
        .map(m => scene.copy(teams = m))

    def -(surface: Surface): Either[EngineError, Scene] =
      removeFromMap(scene.surfaces, surface.id)
        .leftMap(CannotRemoveSurface(_))
        .map(m => scene.copy(surfaces = m))
}