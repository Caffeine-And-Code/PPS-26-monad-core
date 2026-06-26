package engine.core

import engine.core.traits.UpdaterEngine
import engine.model.*

case class Scene(
                  entities: Map[LocatableId, Entity] = Map.empty,
                  surfaces: Map[LocatableId, Surface] = Map.empty,
                  teams: Map[TeamId, Team] = Map.empty
                ) extends UpdaterEngine

object Scene{

  extension (scene: Scene)

    private def getFromMap[K, V](map: Map[K, V], key: K, error: => String): Either[String, V] =
      map.get(key).toRight(error)

    private def addToMap[K, V](
                                map: Map[K, V],
                                key: K,
                                value: V,
                                updateScene: Map[K, V] => Scene
                              ): Either[String, Scene] =
      if (map.contains(key))
        Left(s"Element with key $key already present")
      else
        Right(updateScene(map + (key -> value)))

    private def removeFromMap[K, V](
                                     map: Map[K, V],
                                     key: K,
                                     updateScene: Map[K, V] => Scene
                                   ): Either[String, Scene] =
      if (!map.contains(key))
        Left(s"Element with key $key not present")
      else
        Right(updateScene(map - key))

    // ENTITIES
    infix def getEntity(id: LocatableId): Either[String, Entity] =
      getFromMap(scene.entities, id, s"Entity $id Not Found")

    infix def addEntity(toAdd: Entity): Either[String, Scene] =
      addToMap(scene.entities, toAdd.id, toAdd, m => scene.copy(entities = m))

    infix def removeEntity(id: LocatableId): Either[String, Scene] =
      removeFromMap(scene.entities, id, m => scene.copy(entities = m))

    // TEAMS
    infix def getTeam(id: TeamId): Either[String, Team] =
      getFromMap(scene.teams, id, s"Team $id Not Found")

    infix def addTeam(toAdd: Team): Either[String, Scene] =
      addToMap(scene.teams, toAdd.id, toAdd, m => scene.copy(teams = m))

    infix def removeTeam(id: TeamId): Either[String, Scene] =
      removeFromMap(scene.teams, id, m => scene.copy(teams = m))

    // SURFACES
    infix def getSurface(id: LocatableId): Either[String, Surface] =
      getFromMap(scene.surfaces, id, s"Surface $id Not Found")

    infix def addSurface(toAdd: Surface): Either[String, Scene] =
      addToMap(scene.surfaces, toAdd.id, toAdd, m => scene.copy(surfaces = m))

    infix def removeSurface(id: LocatableId): Either[String, Scene] =
      removeFromMap(scene.surfaces, id, m => scene.copy(surfaces = m))
}