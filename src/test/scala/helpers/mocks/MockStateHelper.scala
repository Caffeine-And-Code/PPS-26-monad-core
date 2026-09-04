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

  /**
   * Returns the default width and height of mocked world bounds.
   *
   * @return
   *   default world dimension
   */
  private def DefaultDimension = 100

  /**
   * Configures a state mock to return stable world bounds.
   *
   * @param state
   *   state mock to configure
   * @param bounds
   *   returned world bounds
   */
  private def mockBounds(state: State, bounds: WorldBounds): Unit =
    (() => state.bounds)
      .expects()
      .returning(bounds)
      .anyNumberOfTimes()

  /**
   * Builds a recursive state mock with configurable collections and removal behaviour.
   *
   * @param entities
   *   initial entities
   * @param teams
   *   initial teams
   * @param surfaces
   *   initial surfaces
   * @param bounds
   *   returned world bounds
   * @param removeEntities
   *   whether successful removals update the recursive mock state
   * @return
   *   configured state mock
   */
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

  /**
   * Creates a state containing the supplied entities.
   *
   * @param entities
   *   fixture entities
   * @return
   *   configured state mock
   */
  def stateWithEntities(entities: List[Entity]): State =
    stateWith(entities)

  /**
   * Creates a state whose entity removals preserve the original collection.
   *
   * @param entities
   *   fixture entities
   * @return
   *   configured non-removing state mock
   */
  def stateWithEntitiesNotRemoving(entities: List[Entity]): State =
    stateWith(
      entities = entities,
      removeEntities = false
    )

  /**
   * Creates a state containing the supplied entities and teams.
   *
   * @param entities
   *   fixture entities
   * @param teams
   *   fixture teams
   * @return
   *   configured state mock
   */
  def stateWithTeams(
      entities: List[Entity],
      teams: List[Team]
  ): State =
    stateWith(
      entities = entities,
      teams = teams
    )

  /**
   * Creates a state containing the supplied entities and surfaces.
   *
   * @param entities
   *   fixture entities
   * @param surfaces
   *   fixture surfaces
   * @return
   *   configured state mock
   */
  def stateWithSurfaces(
      entities: List[Entity],
      surfaces: List[Surface]
  ): State =
    stateWith(
      entities = entities,
      surfaces = surfaces
    )

  /**
   * Creates an otherwise empty state with the supplied world bounds.
   *
   * @param bounds
   *   fixture world bounds
   * @return
   *   configured state mock
   */
  def stateWithBounds(bounds: WorldBounds): State =
    stateWith(
      entities = List.empty,
      bounds = bounds
    )
