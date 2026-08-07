package monad_core.simulator.application.ai

import monad_core.simulator.application.engine.world.World
import monad_core.simulator.domain.ai.agent_evaluation.*
import monad_core.simulator.errors.BaseError
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgentEvaluatorTest extends AnyFunSuite with Matchers with MockFactory:

  private case class EvaluationError() extends BaseError("evaluation failed")

  private class FakeAgentEvaluator(responses: Seq[Either[BaseError, AgentEvaluationResponse]]) extends AgentEvaluator:
    private val responseIterator = responses.iterator

    override def evaluateCase(agentEvaluationTest: AgentEvaluationTest): Either[BaseError, AgentEvaluationResponse] =
      responseIterator.next()

  private def evaluationTest: AgentEvaluationTest =
    AgentEvaluationTest(
      initialWorld = mock[World],
      prompt = "Create a circle",
      language = AgentEvaluationLanguage.English,
      toolCalls = Seq(ToolCall.CreateCircleEntity("circle-1", 10, 20, 5)),
      expectation = "A circle is created"
    )

  test("can evaluate agent responses"):
    val response = AgentEvaluationResponse(
      correctLanguageChoose = AgentEvaluationResult.Bool(true),
      languageCorrectness = AgentEvaluationResult.Score(80),
      correctToolCalls = AgentEvaluationResult.CorrectChooses(4, 5),
      correctToolParams = AgentEvaluationResult.CorrectChooses(3, 5),
      expectationMaintained = AgentEvaluationResult.Bool(false)
    )
    val evaluator = FakeAgentEvaluator(Seq(Right(response)))

    val result = evaluator.evaluate(Seq(evaluationTest))

    result.correctLanguageChoose shouldBe 100
    result.languageCorrectness shouldBe 80
    result.correctToolCalls shouldBe 80
    result.correctToolParams shouldBe 60
    result.expectationMaintained shouldBe 0
    result.evaluationFailed shouldBe 0

  test("can count failed evaluations"):
    val response = AgentEvaluationResponse(
      correctLanguageChoose = AgentEvaluationResult.Bool(true),
      languageCorrectness = AgentEvaluationResult.Score(80),
      correctToolCalls = AgentEvaluationResult.CorrectChooses(1, 1),
      correctToolParams = AgentEvaluationResult.CorrectChooses(1, 1),
      expectationMaintained = AgentEvaluationResult.Bool(true)
    )
    val evaluator = FakeAgentEvaluator(Seq(Right(response), Left(EvaluationError())))

    val result = evaluator.evaluate(Seq(evaluationTest, evaluationTest))

    result.evaluationFailed shouldBe 1
    result.correctLanguageChoose shouldBe 100

  test("evaluating no tests returns empty scores"):
    val evaluator = FakeAgentEvaluator(Seq.empty)

    val result = evaluator.evaluate(Seq.empty)

    result shouldBe AgentEvaluationRecup(0, 0, 0, 0, 0, 0)
