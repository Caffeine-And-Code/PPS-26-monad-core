package monad_core.simulator.domain.ai.agent_evaluator

import monad_core.simulator.domain.ai.agent_evaluation.ToolCall
import org.scalatest.Inside.inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ToolCallTest extends AnyFunSuite with Matchers:

  test("can create a CreateCircle ToolCall"):
    val expectedId = "circle-1"

    val result = ToolCall.CreateCircle(expectedId)

    inside(result):
      case ToolCall.CreateCircle(id) => id shouldBe expectedId
