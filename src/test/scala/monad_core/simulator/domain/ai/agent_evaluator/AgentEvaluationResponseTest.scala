package monad_core.simulator.domain.ai.agent_evaluator

import monad_core.simulator.domain.ai.agent_evaluation.{
  AgentEvaluationResponse,
  AgentEvaluationResult
}
import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgentEvaluationResponseTest extends AnyFunSuite with Matchers:

  test("can create an AgentEvaluationResponse"):
    val correctLanguageChoose: AgentEvaluationResult.Bool = AgentEvaluationResult.Bool(true)
    val languageCorrectness: AgentEvaluationResult.Score =
      AgentEvaluationResult.fromScore(80).value
    val correctToolCalls: AgentEvaluationResult.CorrectChooses =
      AgentEvaluationResult.fromCorrectChooses(4, 5).value
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
