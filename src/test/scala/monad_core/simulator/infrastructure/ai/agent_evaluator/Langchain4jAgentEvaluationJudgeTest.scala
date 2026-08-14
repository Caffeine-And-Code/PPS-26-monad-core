package monad_core.simulator.infrastructure.ai.agent_evaluator

import dev.langchain4j.model.chat.ChatModel
import monad_core.engine.core.Scene
import monad_core.simulator.domain.ai.agent_evaluation.{
  AgentEvaluationLanguage,
  AgentEvaluationResult,
  AgentEvaluationTest
}
import monad_core.simulator.infrastructure.engine.MonadCoreWorld
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Langchain4jAgentEvaluationJudgeTest extends AnyFunSuite with Matchers with MockFactory:

  private val prompt              = "Create a circle"
  private val secondPrompt        = "Change its radius"
  private val expectation         = "The circle is created"
  private val agentResponse       = "The circle was created"
  private val secondAgentResponse = "The radius was changed"

  test("can evaluate an agent response"):
    val assistant = mock[Langchain4jAgentEvaluationJudgeAssistant]
    val judge     = Langchain4jAgentEvaluationJudge(assistant)
    val test      = evaluationTest
    val world     = MonadCoreWorld()
    val judgeResponse = evaluationResult(
      correctLanguageChoose = true,
      languageCorrectness = 80,
      expectationMaintained = true
    )

    assistant.evaluate
      .expects(
        AgentEvaluationLanguage.English.toString,
        s"1: $prompt\n2: $secondPrompt",
        expectation,
        s"1: $agentResponse\n2: $secondAgentResponse",
        "none",
        "none",
        "none"
      )
      .returns(judgeResponse)
      .once()

    val result = judge.evaluate(test, Seq(agentResponse, secondAgentResponse), world).value

    result.correctLanguageChoose shouldBe AgentEvaluationResult.Bool(true)
    result.languageCorrectness shouldBe AgentEvaluationResult.Score(80)
    result.expectationMaintained shouldBe AgentEvaluationResult.Bool(true)

  test("cannot evaluate an invalid judge response"):
    val assistant = mock[Langchain4jAgentEvaluationJudgeAssistant]
    val judge     = Langchain4jAgentEvaluationJudge(assistant)

    assistant.evaluate
      .expects(*, *, *, *, *, *, *)
      .throws(new RuntimeException("invalid response"))
      .once()

    val result = judge.evaluate(
      evaluationTest,
      Seq(agentResponse, secondAgentResponse),
      MonadCoreWorld()
    )

    result shouldBe a[Left[InvalidAgentEvaluationJudgement, ?]]

  test("can build a Langchain4jAgentEvaluationJudge"):
    val chatModel = mock[ChatModel]

    val result = Langchain4jAgentEvaluationJudge.build(chatModel)

    result shouldBe a[Langchain4jAgentEvaluationJudge]

  private def evaluationTest: AgentEvaluationTest =
    AgentEvaluationTest(
      initialScene = Scene(),
      prompts = Seq(prompt, secondPrompt),
      language = AgentEvaluationLanguage.English,
      toolCalls = Seq.empty,
      expectation = expectation
    )

  private def evaluationResult(
      correctLanguageChoose: Boolean,
      languageCorrectness: Int,
      expectationMaintained: Boolean
  ): Langchain4jAgentEvaluationResult =
    val result = Langchain4jAgentEvaluationResult()
    result.correctLanguageChoose = correctLanguageChoose
    result.languageCorrectness = languageCorrectness
    result.expectationMaintained = expectationMaintained
    result
