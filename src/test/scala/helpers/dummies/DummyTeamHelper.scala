package helpers.dummies

import monad_core.engine.model.{Entity, Team}
import org.scalatest.EitherValues.convertEitherToValuable

/** A helper object for creating dummy teams for testing purposes. */
object DummyTeamHelper:

  def addTeam(entity: Entity, teamId: String): Entity =
    entity.withTeamId(teamId).value

  def makeTeam(id: String, enemies: Set[String] = Set.empty): Team =
    Team.create(id, enemies).value
