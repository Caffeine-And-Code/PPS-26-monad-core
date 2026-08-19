package monad_core.engine.physics.helper

import monad_core.engine.core.traits.State
import monad_core.engine.core.{
  CannotAddAlreadyPresentElementInMap,
  CannotAddEntity,
  CannotRemoveEntity,
  CannotRemoveNonPresentElementFromMap
}
import monad_core.engine.model.{Entity, Surface, Team}
import org.scalamock.scalatest.MockFactory

private[physics] trait PhysicsSceneHelper:

  self: MockFactory =>

  def sceneWithEntities(entities: List[Entity]): State =
    val scene = mock[State]

    (() => scene.allEntities)
      .expects()
      .returning(entities)
      .anyNumberOfTimes()

    scene.removeEntity
      .expects(*)
      .onCall { (entity: Entity) =>
        val isInScene = scene.allEntities.exists(_.id == entity.id)

        if (isInScene) {
          val updatedEntities = scene.allEntities.filterNot(_.id == entity.id)

          Right(sceneWithEntities(updatedEntities))
        } else {
          Left(CannotRemoveEntity(CannotRemoveNonPresentElementFromMap(entity.id)))
        }
      }
      .anyNumberOfTimes()

    scene.addEntity
      .expects(*)
      .onCall { (entity: Entity) =>
        val isInScene = scene.allEntities.exists(_.id == entity.id)

        if (isInScene) {
          Left(CannotAddEntity(CannotAddAlreadyPresentElementInMap(entity.id)))
        } else {
          val updatedEntities = scene.allEntities :+ entity

          Right(sceneWithEntities(updatedEntities))
        }
      }
      .anyNumberOfTimes()

    scene

  def sceneWithEntitiesNotRemoving(entities: List[Entity]): State =
    val scene = mock[State]

    (() => scene.allEntities)
      .expects()
      .returning(entities)
      .anyNumberOfTimes()

    scene.removeEntity
      .expects(*)
      .onCall { (entity: Entity) =>
        Right(sceneWithEntitiesNotRemoving(entities))
      }
      .anyNumberOfTimes()

    scene.addEntity
      .expects(*)
      .onCall { (entity: Entity) =>
        val isInScene = scene.allEntities.exists(_.id == entity.id)

        if (isInScene) {
          Left(CannotAddEntity(CannotAddAlreadyPresentElementInMap(entity.id)))
        } else {
          val updatedEntities = scene.allEntities :+ entity

          Right(sceneWithEntitiesNotRemoving(updatedEntities))
        }
      }
      .anyNumberOfTimes()

    scene

  def sceneWithTeams(
      entities: List[Entity],
      teams: List[Team]
  ): State =
    val scene = mock[State]

    (() => scene.allEntities)
      .expects()
      .returning(entities)
      .anyNumberOfTimes()

    (() => scene.allTeams)
      .expects()
      .returning(teams)
      .anyNumberOfTimes()

    scene.removeEntity
      .expects(*)
      .onCall { (entity: Entity) =>
        val isInScene = scene.allEntities.exists(_.id == entity.id)

        if (isInScene) {
          val updatedEntities = scene.allEntities.filterNot(_.id == entity.id)

          Right(sceneWithTeams(updatedEntities, teams))
        } else {
          Left(CannotRemoveEntity(CannotRemoveNonPresentElementFromMap(entity.id)))
        }
      }
      .anyNumberOfTimes()

    scene.addEntity
      .expects(*)
      .onCall { (entity: Entity) =>
        val isInScene = scene.allEntities.exists(_.id == entity.id)

        if (isInScene) {
          Left(CannotAddEntity(CannotAddAlreadyPresentElementInMap(entity.id)))
        } else {
          val updatedEntities = scene.allEntities :+ entity

          Right(sceneWithTeams(updatedEntities, teams))
        }
      }
      .anyNumberOfTimes()

    scene

  def sceneWithSurfaces(
      entities: List[Entity],
      surfaces: List[Surface]
  ): State =
    val scene = mock[State]

    (() => scene.allEntities)
      .expects()
      .returning(entities)
      .anyNumberOfTimes()

    (() => scene.allSurfaces)
      .expects()
      .returning(surfaces)
      .anyNumberOfTimes()

    scene.removeEntity
      .expects(*)
      .onCall { (entity: Entity) =>
        val isInScene = scene.allEntities.exists(_.id == entity.id)

        if (isInScene) {
          val updatedEntities = scene.allEntities.filterNot(_.id == entity.id)

          Right(sceneWithSurfaces(updatedEntities, surfaces))
        } else {
          Left(CannotRemoveEntity(CannotRemoveNonPresentElementFromMap(entity.id)))
        }
      }
      .anyNumberOfTimes()

    scene.addEntity
      .expects(*)
      .onCall { (entity: Entity) =>
        val isInScene = scene.allEntities.exists(_.id == entity.id)

        if (isInScene) {
          Left(CannotAddEntity(CannotAddAlreadyPresentElementInMap(entity.id)))
        } else {
          val updatedEntities = scene.allEntities :+ entity

          Right(sceneWithSurfaces(updatedEntities, surfaces))
        }
      }
      .anyNumberOfTimes()

    scene
