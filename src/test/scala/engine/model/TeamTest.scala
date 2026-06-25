package engine.model

import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TeamTest extends AnyFunSuite with Inside with Matchers:

  val ValidTeamId = "team1"
  val ValidEnemy = "team2"

  test("can create a team"):
    val enemyTwo = "team3"

    val team = Team.create(ValidTeamId, Set(ValidEnemy, enemyTwo))

    inside(team):
      case Right(t) =>
        t.teamId.value shouldBe ValidTeamId
        t.enemies.size shouldBe 2
        t.enemies.map(_.value) should contain allOf(ValidEnemy, enemyTwo)

  test("cannot create a team where the team his self is the team enemy"):
    val team = Team.create(ValidTeamId, Set(ValidTeamId))

    team shouldBe Left("A team cannot be its own enemy")

  test("can add an enemy to the team"):
    val team = for {
      team <- Team.create(ValidTeamId)
      team <- team.addEnemy(ValidEnemy)
    } yield team

    inside(team):
      case Right(t) =>
        t.enemies.size shouldBe 1
        t.enemies.map(_.value) should contain (ValidEnemy)

  test("cannot add team teamId as enemy to the team"):
    val team = for {
      team <- Team.create(ValidTeamId)
      team <- team.addEnemy(ValidTeamId)
    } yield team

    team shouldBe Left("A team cannot be its own enemy")

  test("can remove a enemy"):
    val team = for {
      team <- Team.create(ValidTeamId, Set(ValidEnemy))
    } yield team.removeEnemy(ValidEnemy)

    inside(team):
      case Right(t) =>
        t.enemies.size shouldBe 0