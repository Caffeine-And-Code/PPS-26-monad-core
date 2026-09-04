package monad_core.engine.model

import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TeamIdTest extends AnyFunSuite with Matchers:

  test("can create an optional team ID from a valid value"):
    val validTeamId = Some("team1")

    val teamId = TeamId.fromOption(validTeamId)

    teamId.value.map(_.value) shouldBe validTeamId

  test("cannot create an optional team ID from an invalid value"):
    val invalidTeamId = Some("   ")

    val teamId = TeamId.fromOption(invalidTeamId)

    teamId shouldBe Left(TeamIdCannotBeEmpty())
