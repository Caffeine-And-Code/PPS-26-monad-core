package monad_core.simulator.domain.ai.agent_evaluator

import monad_core.simulator.application.engine.world.World
import monad_core.simulator.domain.ai.agent_evaluation.{AgentEvaluationLanguage, AgentEvaluationTest, ToolCall}
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgentEvaluationTestTest extends AnyFunSuite with Matchers with MockFactory:

  test("can create an AgentEvaluationTest"):
    val initialWorld = mock[World]
    val prompt = "Create a circle"
    val language = AgentEvaluationLanguage.English
    val toolCalls = Seq(ToolCall.CreateCircleEntity("circle-1", 10, 20, 5))
    val expectation = "A circle is created"

    val result = AgentEvaluationTest(initialWorld, prompt, language, toolCalls, expectation)

    result.initialWorld shouldBe initialWorld
    result.prompt shouldBe prompt
    result.language shouldBe language
    result.toolCalls shouldBe toolCalls
    result.expectation shouldBe expectation

  test("can create the supported AgentEvaluationLanguages"):
    AgentEvaluationLanguage.Italian shouldBe AgentEvaluationLanguage.Italian
    AgentEvaluationLanguage.English shouldBe AgentEvaluationLanguage.English
