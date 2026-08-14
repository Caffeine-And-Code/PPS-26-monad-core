package monad_core.simulator.domain.ai.agent_evaluator

import monad_core.engine.core.Scene
import monad_core.simulator.domain.ai.agent_evaluation.{
  AgentEvaluationLanguage,
  AgentEvaluationTest,
  ToolCall
}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgentEvaluationTestTest extends AnyFunSuite with Matchers:

  test("can create an AgentEvaluationTest"):
    val initialScene = Scene()
    val prompts      = Seq("Create a circle", "Change its radius")
    val language     = AgentEvaluationLanguage.English
    val toolCalls    = Seq(ToolCall.CreateCircleEntity("circle-1", 10, 20, 5))
    val expectation  = "A circle is created"

    val result = AgentEvaluationTest(initialScene, prompts, language, toolCalls, expectation)

    result.initialScene shouldBe initialScene
    result.prompts shouldBe prompts
    result.language shouldBe language
    result.toolCalls shouldBe toolCalls
    result.expectation shouldBe expectation

  test("can create the supported AgentEvaluationLanguages"):
    AgentEvaluationLanguage.Italian shouldBe AgentEvaluationLanguage.Italian
    AgentEvaluationLanguage.English shouldBe AgentEvaluationLanguage.English
