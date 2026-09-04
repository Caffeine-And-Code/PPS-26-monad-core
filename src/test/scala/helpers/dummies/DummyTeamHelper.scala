package helpers.dummies

import monad_core.engine.model.{Entity, Team}
import org.scalatest.EitherValues.convertEitherToValuable

/** A helper object for creating dummy teams for testing purposes. */
object DummyTeamHelper:

  /**
   * Assigns a validated team identifier to an entity.
   *
   * @param entity
   *   entity to update
   * @param teamId
   *   team identifier
   * @return
   *   entity assigned to the team
   */
  def addTeam(entity: Entity, teamId: String): Entity =
    entity.withTeamId(Some(teamId)).value

  /**
   * Creates a validated team with the supplied enemy identifiers.
   *
   * @param id
   *   team identifier
   * @param enemies
   *   enemy team identifiers
   * @return
   *   validated team fixture
   */
  def makeTeam(id: String, enemies: Set[String] = Set.empty): Team =
    Team.create(id, enemies).value
