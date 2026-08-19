package monad_core.engine.physics.helper

import monad_core.engine.model.{Entity, Team}
import org.scalatest.EitherValues.convertEitherToValuable

private[physics] object PhysicsTeamHelper:

  def addTeam(entity: Entity, teamId: String): Entity =
    entity.withTeamId(teamId).value

  def makeTeam(id: String, enemies: Set[String] = Set.empty): Team =
    Team.create(id, enemies).value
