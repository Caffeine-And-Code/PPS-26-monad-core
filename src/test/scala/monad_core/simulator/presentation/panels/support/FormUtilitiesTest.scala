package monad_core.simulator.presentation.panels.support

import helpers.arrangers.MonadCoreTeamArranger
import monad_core.simulator.TeamNotFoundDuringSelection
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.domain.engine.MonadCoreTeam
import monad_core.simulator.presentation.panels.support.FormUtilities
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.should

class FormUtilitiesTest extends AnyFunSuite with Matchers with MockFactory:

  test("onActionMakeSnapshot execute the given action and then creates a snapshot of the current world"):

    given runtime: GameEngineRuntime = mock[GameEngineRuntime]

    val expectedActionInput: String = "a"
    val action = mockFunction[String, Unit]

    inSequence:
      action.expects(expectedActionInput).once()
      (() => runtime.createSnapshot()).expects().once()

    FormUtilities.onActionMakeSnapshot(expectedActionInput, action)

  test("getTeamsSafely returns the populated team list when the provided world return Right"):
    val world: World = mock[World]
    val expectedTeams: List[MonadCoreTeam] = MonadCoreTeamArranger.arrangeTeams.toList

    (() => world.getAllTeams).expects().returns(Right(expectedTeams))
    
    val result = FormUtilities.getTeamsSafely(world)
    
    result should be(expectedTeams)