package monad_core.simulator.presentation.panels.support

import monad_core.simulator.TeamNotFoundDuringSelection
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.presentation.panels.support.FormUtilities
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.should

class FormUtilitiesTest extends AnyFunSuite with Matchers with MockFactory:

  test(
    "onActionMakeSnapshot execute the given action and then creates a snapshot of the current world"
  ):

    given runtime: GameEngineRuntime = mock[GameEngineRuntime]

    val expectedActionInput: String = "a"
    val action                      = mockFunction[String, Unit]

    inSequence:
      action.expects(expectedActionInput).once()
      (() => runtime.createSnapshot()).expects().once()

    FormUtilities.onActionMakeSnapshot(expectedActionInput, action)
