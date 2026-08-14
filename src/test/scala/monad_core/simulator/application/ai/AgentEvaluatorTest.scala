package monad_core.simulator.application.ai

import monad_core.engine.model.Scene
import monad_core.simulator.domain.ai.agent_evaluation.*
import monad_core.simulator.errors.BaseError
import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgentEvaluatorTest extends AnyFunSuite with Matchers:

  private case class EvaluationError() extends BaseError("evaluation failed")

  private class FakeAgentEvaluator(responses: Seq[Either[BaseError, AgentEvaluationResponse]])
      extends AgentEvaluator:
    private val responseIterator = responses.iterator

    override def evaluateCase(
        agentEvaluationTest: AgentEvaluationTest
    ): Either[BaseError, AgentEvaluationResponse] =
      responseIterator.next()

  private def evaluationTest: AgentEvaluationTest =
    AgentEvaluationTest(
      initialScene = Scene(),
      prompts = Seq("Create a circle"),
      language = AgentEvaluationLanguage.English,
      toolCalls = Seq(ToolCall.CreateCircleEntity("circle-1", 10, 20, 5)),
      expectation = "A circle is created"
    )

  test("can evaluate agent responses"):
    val response = AgentEvaluationResponse(
      correctLanguageChoose = AgentEvaluationResult.Bool(true),
      languageCorrectness = AgentEvaluationResult.fromScore(80).value,
      correctToolCalls = AgentEvaluationResult.fromCorrectChooses(4, 5).value,
      expectationMaintained = AgentEvaluationResult.Bool(false)
    )
    val evaluator = FakeAgentEvaluator(Seq(Right(response)))

    val result = evaluator.evaluate(Seq(evaluationTest))

    result.correctLanguageChoose shouldBe 100
    result.languageCorrectness shouldBe 80
    result.correctToolCalls shouldBe 80
    result.expectationMaintained shouldBe 0
    result.evaluationFailed shouldBe 0

  test("can count failed evaluations"):
    val response = AgentEvaluationResponse(
      correctLanguageChoose = AgentEvaluationResult.Bool(true),
      languageCorrectness = AgentEvaluationResult.fromScore(80).value,
      correctToolCalls = AgentEvaluationResult.fromCorrectChooses(1, 1).value,
      expectationMaintained = AgentEvaluationResult.Bool(true)
    )
    val evaluator = FakeAgentEvaluator(Seq(Right(response), Left(EvaluationError())))

    val result = evaluator.evaluate(Seq(evaluationTest, evaluationTest))

    result.evaluationFailed shouldBe 1
    result.correctLanguageChoose shouldBe 100

  test("evaluating no tests returns empty scores"):
    val evaluator = FakeAgentEvaluator(Seq.empty)

    val result = evaluator.evaluate(Seq.empty)

    result shouldBe AgentEvaluationRecap(0, 0, 0, 0, 0)

  test("an evaluation without expected tool calls has full tool scores"):
    val response = AgentEvaluationResponse(
      correctLanguageChoose = AgentEvaluationResult.Bool(true),
      languageCorrectness = AgentEvaluationResult.fromScore(80).value,
      correctToolCalls = AgentEvaluationResult.fromCorrectChooses(0, 0).value,
      expectationMaintained = AgentEvaluationResult.Bool(true)
    )
    val evaluator = FakeAgentEvaluator(Seq(Right(response)))

    val result = evaluator.evaluate(Seq(evaluationTest))

    result.correctToolCalls shouldBe 100
