package monad_core.engine.helper

import monad_core.engine.core.{
  CannotAddAlreadyPresentElementInMap,
  CannotAddEntity,
  CannotRemoveEntity,
  CannotRemoveNonPresentElementFromMap
}
import monad_core.engine.core.traits.State
import monad_core.engine.model.{Entity, Surface, Team, WorldBounds}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable

private[engine] trait MockSceneHelper:

  self: MockFactory =>

  private def mockBounds(scene: State, bounds: WorldBounds): Unit =
    (() => scene.bounds)
      .expects()
      .returning(bounds)
      .anyNumberOfTimes()

  private def sceneWith(
      entities: List[Entity],
      teams: List[Team] = List.empty,
      surfaces: List[Surface] = List.empty,
      bounds: WorldBounds = WorldBounds(100, 100).value,
      removeEntities: Boolean = true
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

    (() => scene.allSurfaces)
      .expects()
      .returning(surfaces)
      .anyNumberOfTimes()

    scene.removeEntity
      .expects(*)
      .onCall { (entity: Entity) =>
        if !removeEntities then
          Right(
            sceneWith(
              entities = entities,
              teams = teams,
              surfaces = surfaces,
              bounds = bounds,
              removeEntities = false
            )
          )
        else if entities.exists(_.id == entity.id) then
          val updatedEntities = entities.filterNot(_.id == entity.id)

          Right(
            sceneWith(
              entities = updatedEntities,
              teams = teams,
              surfaces = surfaces,
              bounds = bounds,
              removeEntities = removeEntities
            )
          )
        else Left(CannotRemoveEntity(CannotRemoveNonPresentElementFromMap(entity.id)))
      }
      .anyNumberOfTimes()

    scene.addEntity
      .expects(*)
      .onCall { (entity: Entity) =>
        if entities.exists(_.id == entity.id) then
          Left(CannotAddEntity(CannotAddAlreadyPresentElementInMap(entity.id)))
        else
          Right(
            sceneWith(
              entities = entities :+ entity,
              teams = teams,
              surfaces = surfaces,
              bounds = bounds,
              removeEntities = removeEntities
            )
          )
      }
      .anyNumberOfTimes()

    mockBounds(scene, bounds)
    scene

  def sceneWithEntities(entities: List[Entity]): State =
    sceneWith(entities)

  def sceneWithEntitiesNotRemoving(entities: List[Entity]): State =
    sceneWith(
      entities = entities,
      removeEntities = false
    )

  def sceneWithTeams(
      entities: List[Entity],
      teams: List[Team]
  ): State =
    sceneWith(
      entities = entities,
      teams = teams
    )

  def sceneWithSurfaces(
      entities: List[Entity],
      surfaces: List[Surface]
  ): State =
    sceneWith(
      entities = entities,
      surfaces = surfaces
    )

  def sceneWithBounds(bounds: WorldBounds): State =
    sceneWith(
      entities = List.empty,
      bounds = bounds
    )
