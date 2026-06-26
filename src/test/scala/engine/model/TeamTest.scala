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
      case Right(team) =>
        team.id.value shouldBe ValidTeamId
        team.enemies.size shouldBe 2
        team.enemies.map(_.value) should contain allOf(ValidEnemy, enemyTwo)

  test("cannot create a team with invalid team id"):
    val invalidTeamId = "   "

    val team = Team.create(invalidTeamId)

    team shouldBe Left("TeamId cannot be empty")

  test("Cannot create a team where the team it self is it's own enemy"):
    val team = Team.create(ValidTeamId, Set(ValidTeamId))

    team shouldBe Left("A team cannot be its own enemy")

  test("can add an enemy to the team"):
    val team = for {
      team <- Team.create(ValidTeamId)
      team <- team.addEnemy(ValidEnemy)
    } yield team

    inside(team):
      case Right(team) =>
        team.enemies.size shouldBe 1
        team.enemies.map(_.value) should contain(ValidEnemy)

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

  test("removing enemy with invalid team id leave the team not alterated"):
    val invalidTeamId = ""
    val team = for {
      team <- Team.create(ValidTeamId, Set(ValidEnemy))
    } yield team.removeEnemy(invalidTeamId)

    inside(team):
      case Right(team) =>
        team.enemies.size shouldBe 1

  test("removing not existing enemy leave the team not alterated"):
    val enemyTeamId = "team3"
    val team = for {
      team <- Team.create(ValidTeamId, Set(ValidEnemy))
    } yield team.removeEnemy(enemyTeamId)

    inside(team):
      case Right(team) =>
        team.enemies.size shouldBe 1

  test("can check that an enemy is an enemy by team id"):
    val isEnemy = for {
      team <- Team.create(ValidTeamId, Set(ValidEnemy))
    } yield team.isEnemyOf(ValidEnemy)

    inside(isEnemy):
      case Right(value) => value shouldBe true

  test("can check that a not enemy is not an enemy by team id"):
    val enemyTeamId = "team3"
    val isEnemy = for {
      team <- Team.create(ValidTeamId, Set(ValidEnemy))
    } yield team.isEnemyOf(enemyTeamId)

    inside(isEnemy):
      case Right(value) => value shouldBe false


  test("can check that an invalid team id is not an enemy"):
    val invalidTeamId = " "
    val isEnemy = for {
      team <- Team.create(ValidTeamId, Set(ValidEnemy))
    } yield team.isEnemyOf(invalidTeamId)

    inside(isEnemy):
      case Right(value) => value shouldBe false