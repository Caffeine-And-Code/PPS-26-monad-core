package engine.core

import engine.model.{Entity, LocatableId, Surface, Team, TeamId}

case class Scene(
                  entities: Map[LocatableId, Entity] = Map.empty,
                  surfaces: Map[LocatableId, Surface] = Map.empty,
                  teams: Map[TeamId, Team] = Map.empty
                )

object Scene {

  extension (scene: Scene)

    private def getFromMap[V, Key](
                                    map: Map[Key, V],
                                    key: Key,
                                    errorMessage: String
                                  ): Either[String, V] =
      map.get(key).toRight(errorMessage)

    private def operationToElementInMap[V, Key](
                                                 map: Map[Key, V],
                                                 onContains: () => Either[String, Scene],
                                                 onElementNotContained: () => Either[String, Scene],
                                                 keyToOperateOn: Key
                                               ): Either[String, Scene] =
      if (map.contains(keyToOperateOn))
        onContains()
      else
        onElementNotContained()

    private def addToMap[V, Key](
                                  map: Map[Key, V],
                                  setMap: Map[Key, V] => Scene,
                                  key: Key,
                                  toAdd: V
                                ): Either[String, Scene] = {
      operationToElementInMap(
        map = map,
        onContains = () => Left(s"Element with $key already present"),
        onElementNotContained = () => Right(setMap(map + (key -> toAdd))),
        keyToOperateOn = key
      )
    }

    private def removeToMap[V, Key](
                                     map: Map[Key, V],
                                     setMap: Map[Key, V] => Scene,
                                     keyToRemove: Key,
                                   ): Either[String, Scene] =
      operationToElementInMap(
        map = map,
        onContains = () => Right(setMap(map.removed(keyToRemove))),
        onElementNotContained = () => Left(s"Element with $keyToRemove not present"),
        keyToOperateOn = keyToRemove
      )

    infix def getEntity(entityId: LocatableId): Either[String, Entity] =
      getFromMap(scene.entities, entityId, "Entity Not Found")

    infix def addEntity(toAdd: Entity): Either[String, Scene] =
      addToMap(scene.entities, m => scene.copy(entities = m), toAdd.id, toAdd)
      
    infix def removeEntity(keyToRemove: LocatableId):Either[String, Scene] =
      removeToMap(scene.entities, m => scene.copy(entities = m), keyToRemove)
    
    infix def getTeam(teamId: TeamId): Either[String, Team] =
      getFromMap(scene.teams, teamId, "Team Not Found")

    infix def addTeam(toAdd: Team): Either[String, Scene] =
      addToMap(scene.teams, m => scene.copy(teams = m), toAdd.id, toAdd)
      
    infix def removeTeam(keyToRemove: TeamId):Either[String, Scene] =
      removeToMap(scene.teams, m => scene.copy(teams = m), keyToRemove)
}
