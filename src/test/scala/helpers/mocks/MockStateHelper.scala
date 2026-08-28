package helpers.mocks

import monad_core.engine.core.traits.State
import monad_core.engine.core.{
  CannotAddAlreadyPresentElementInMap,
  CannotAddEntity,
  CannotRemoveEntity,
  CannotRemoveNonPresentElementFromMap
}
import monad_core.engine.model.{Entity, Surface, Team, WorldBounds}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable

/** A trait for creating mock states for testing purposes. */
trait MockStateHelper:

  self: MockFactory =>

  private def DefaultDimension = 100
  
  private def mockBounds(state: State, bounds: WorldBounds): Unit =
    (() => state.bounds)
      .expects()
      .returning(bounds)
      .anyNumberOfTimes()

  private def stateWith(
      entities: List[Entity],
      teams: List[Team] = List.empty,
      surfaces: List[Surface] = List.empty,
      bounds: WorldBounds = WorldBounds(DefaultDimension, DefaultDimension).value,
      removeEntities: Boolean = true
  ): State =
    val state = mock[State]

    (() => state.allEntities)
      .expects()
      .returning(entities)
      .anyNumberOfTimes()

    (() => state.allTeams)
      .expects()
      .returning(teams)
      .anyNumberOfTimes()

    (() => state.allSurfaces)
      .expects()
      .returning(surfaces)
      .anyNumberOfTimes()

    state.removeEntity
      .expects(*)
      .onCall { (entity: Entity) =>
        if !removeEntities then
          Right(
            stateWith(
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
            stateWith(
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

    state.addEntity
      .expects(*)
      .onCall { (entity: Entity) =>
        if entities.exists(_.id == entity.id) then
          Left(CannotAddEntity(CannotAddAlreadyPresentElementInMap(entity.id)))
        else
          Right(
            stateWith(
              entities = entities :+ entity,
              teams = teams,
              surfaces = surfaces,
              bounds = bounds,
              removeEntities = removeEntities
            )
          )
      }
      .anyNumberOfTimes()

    mockBounds(state, bounds)
    state

  def stateWithEntities(entities: List[Entity]): State =
    stateWith(entities)

  def stateWithEntitiesNotRemoving(entities: List[Entity]): State =
    stateWith(
      entities = entities,
      removeEntities = false
    )

  def stateWithTeams(
      entities: List[Entity],
      teams: List[Team]
  ): State =
    stateWith(
      entities = entities,
      teams = teams
    )

  def stateWithSurfaces(
      entities: List[Entity],
      surfaces: List[Surface]
  ): State =
    stateWith(
      entities = entities,
      surfaces = surfaces
    )

  def stateWithBounds(bounds: WorldBounds): State =
    stateWith(
      entities = List.empty,
      bounds = bounds
    )
