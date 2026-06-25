package engine.model

import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TeamTest extends AnyFunSuite with Inside with Matchers:

  val ValidTeamId = "team1"

  test("can create a team"):
    val enemyOne = "team2"
    val enemyTwo = "team3"

    val team = for {
      teamId <- TeamId(ValidTeamId)
      enemy1 <- TeamId(enemyOne)
      enemy2 <- TeamId(enemyTwo)
      team <- Team(teamId, Set(enemy1, enemy2))
    } yield team

    inside(team):
      case Right(t) =>
        t.teamId.value shouldBe ValidTeamId
        t.enemies.size shouldBe 2
        t.enemies.map(_.value) should contain allOf(enemyOne, enemyTwo)

