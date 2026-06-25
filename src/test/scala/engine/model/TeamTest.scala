package engine.model

import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TeamTest extends AnyFunSuite with Inside with Matchers:

  val ValidTeamId = "team1"
  val ValidEnemy = "team2"

  test("can create a team"):
    val enemyTwo = "team3"

    val team = for {
      teamId <- TeamId(ValidTeamId)
      enemy1 <- TeamId(ValidEnemy)
      enemy2 <- TeamId(enemyTwo)
      team <- Team(teamId, Set(enemy1, enemy2))
    } yield team

    inside(team):
      case Right(t) =>
        t.teamId.value shouldBe ValidTeamId
        t.enemies.size shouldBe 2
        t.enemies.map(_.value) should contain allOf(ValidEnemy, enemyTwo)

  test("cannot create a team where he is the team enemy"):
    val team = for {
      teamId <- TeamId(ValidTeamId)
      enemy1 <- TeamId(ValidTeamId)
      team   <- Team(teamId, Set(enemy1))
    } yield team

    team.isLeft shouldBe true

  test("can add an enemy to the team"):
    val team = for {
      teamId <- TeamId(ValidTeamId)
      team <- Team(teamId, Set())
      enemy <- TeamId(ValidEnemy)
      team <- team.addEnemy(enemy)
    } yield team

    inside(team):
      case Right(t) =>
        t.enemies.size shouldBe 1
        t.enemies.map(_.value) should contain (ValidEnemy)
