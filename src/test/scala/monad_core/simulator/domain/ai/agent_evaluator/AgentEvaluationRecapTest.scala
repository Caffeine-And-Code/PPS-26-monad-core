package monad_core.simulator.domain.ai.agent_evaluator

import monad_core.simulator.domain.ai.agent_evaluation.AgentEvaluationRecap
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgentEvaluationRecapTest extends AnyFunSuite with Matchers:

  test("can create an AgentEvaluationRecup"):
    val result = AgentEvaluationRecap(
      correctLanguageChoose = 100,
      languageCorrectness = 80,
      correctToolCalls = 75,
      expectationMaintained = 100,
      evaluationFailed = 2
    )

    result.correctLanguageChoose shouldBe 100
    result.languageCorrectness shouldBe 80
    result.correctToolCalls shouldBe 75
    result.expectationMaintained shouldBe 100
    result.evaluationFailed shouldBe 2
