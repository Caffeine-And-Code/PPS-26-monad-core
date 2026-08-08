package monad_core.simulator.domain.ai.agent_evaluator

import monad_core.simulator.domain.ai.agent_evaluation.{AgentEvaluationResponse, AgentEvaluationResult}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgentEvaluationResponseTest extends AnyFunSuite with Matchers:

  test("can create an AgentEvaluationResponse"):
    val correctLanguageChoose: AgentEvaluationResult.Bool = AgentEvaluationResult.Bool(true)
    val languageCorrectness: AgentEvaluationResult.Score = AgentEvaluationResult.Score(80)
    val correctToolCalls: AgentEvaluationResult.CorrectChooses = AgentEvaluationResult.CorrectChooses(4, 5)
    val correctToolParams: AgentEvaluationResult.CorrectChooses = AgentEvaluationResult.CorrectChooses(3, 5)
    val expectationMaintained: AgentEvaluationResult.Bool = AgentEvaluationResult.Bool(false)

    val result = AgentEvaluationResponse(
      correctLanguageChoose,
      languageCorrectness,
      correctToolCalls,
      expectationMaintained
    )

    result.correctLanguageChoose shouldBe correctLanguageChoose
    result.languageCorrectness shouldBe languageCorrectness
    result.correctToolCalls shouldBe correctToolCalls
    result.expectationMaintained shouldBe expectationMaintained
