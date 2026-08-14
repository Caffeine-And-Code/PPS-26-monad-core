package monad_core.simulator.infrastructure.ai.agent_evaluator

import dev.langchain4j.agent.tool.ToolExecutionRequest
import dev.langchain4j.invocation.InvocationContext
import dev.langchain4j.service.Result
import dev.langchain4j.service.tool.{ToolExecution, ToolExecutionResult}
import monad_core.engine.core.Scene
import monad_core.simulator.application.engine.EngineControl
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.application.logging.Logger
import monad_core.simulator.domain.ai.ConversationId
import monad_core.simulator.domain.ai.agent_evaluation.*
import monad_core.simulator.infrastructure.ai.{
  Langchain4jAssistant,
  Langchain4jAssistantBuilder,
  Langchain4jOllamaConfig
}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.*
import org.scalatest.OptionValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*

class Langchain4jAgentEvaluatorTest extends AnyFunSuite with Matchers with MockFactory:

  private val prompt              = "Create a circle"
  private val secondPrompt        = "Create another circle"
  private val agentResponse       = "The circle was created"
  private val secondAgentResponse = "The second circle was created"
  private val circleId            = "circle-1"
  private val secondCircleId      = "circle-2"
  private val x                   = 10.0
  private val y                   = 20.0
  private val expectedRadius      = 5.0
  private val conversationId      = ConversationId.from("agent-evaluation").value

  test("can evaluate an agent case"):
    val assistantBuilder = mock[Langchain4jAssistantBuilder]
    val assistant        = mock[Langchain4jAssistant]
    val judge            = mock[Langchain4jAgentEvaluationJudge]
    val logger           = mock[Logger]
    val initialScene     = Scene()
    val expectedToolCall: ToolCall.CreateCircleEntity =
      ToolCall.CreateCircleEntity(circleId, x, y, expectedRadius)
    val test               = evaluationTest(initialScene, Seq(expectedToolCall))
    val resultWithToolCall = assistantResult(Seq(toolExecution(expectedToolCall)))
    val judgement          = successfulJudgement
    var evaluationWorld    = Option.empty[World]
    given Logger           = logger
    val evaluator = Langchain4jAgentEvaluator(assistantBuilder, Langchain4jToolCallMapper(), judge)

    assistantBuilder.build
      .expects(*, *)
      .onCall: (world: World, _: EngineControl) =>
        evaluationWorld = Some(world)
        assistant
    assistant.chat.expects(conversationId, prompt).returns(resultWithToolCall).once()
    judge.evaluate.expects(test, Seq(agentResponse), *).returns(Right(judgement)).once()
    logger.info.expects(completedLog("success", prompts = 1, expectedToolCalls = 1)).once()

    val result = evaluator.evaluateCase(test).value

    result.correctLanguageChoose shouldBe judgement.correctLanguageChoose
    result.languageCorrectness shouldBe judgement.languageCorrectness
    result.correctToolCalls shouldBe AgentEvaluationResult.fromCorrectChooses(1, 1).value
    result.expectationMaintained shouldBe judgement.expectationMaintained
    evaluationWorld.value.scene shouldBe initialScene

  test("can evaluate an agent case without tool calls"):
    val assistantBuilder = mock[Langchain4jAssistantBuilder]
    val assistant        = mock[Langchain4jAssistant]
    val judge            = mock[Langchain4jAgentEvaluationJudge]
    val logger           = mock[Logger]
    val initialScene     = Scene()
    val test             = evaluationTest(initialScene, Seq.empty)
    val judgement        = successfulJudgement
    given Logger         = logger
    val evaluator = Langchain4jAgentEvaluator(assistantBuilder, Langchain4jToolCallMapper(), judge)

    assistantBuilder.build.expects(*, *).returns(assistant).once()
    assistant.chat.expects(conversationId, prompt).returns(assistantResult(Seq.empty)).once()
    judge.evaluate.expects(test, Seq(agentResponse), *).returns(Right(judgement)).once()
    logger.info.expects(completedLog("success", prompts = 1, expectedToolCalls = 0)).once()

    val result = evaluator.evaluateCase(test).value

    result.correctToolCalls shouldBe AgentEvaluationResult.fromCorrectChooses(0, 0).value

  test("a tool call with incorrect arguments is not correct"):
    val assistantBuilder = mock[Langchain4jAssistantBuilder]
    val assistant        = mock[Langchain4jAssistant]
    val judge            = mock[Langchain4jAgentEvaluationJudge]
    val logger           = mock[Logger]
    val expectedToolCall: ToolCall.CreateCircleEntity =
      ToolCall.CreateCircleEntity(circleId, x, y, expectedRadius)
    val actualToolCall: ToolCall.CreateCircleEntity =
      ToolCall.CreateCircleEntity("wrong-circle", x, y, radius = 100)
    val test      = evaluationTest(Scene(), Seq(expectedToolCall))
    val judgement = successfulJudgement
    given Logger  = logger
    val evaluator = Langchain4jAgentEvaluator(assistantBuilder, Langchain4jToolCallMapper(), judge)

    assistantBuilder.build.expects(*, *).returns(assistant).once()
    assistant.chat
      .expects(conversationId, prompt)
      .returns(assistantResult(Seq(toolExecution(actualToolCall))))
      .once()
    judge.evaluate.expects(test, Seq(agentResponse), *).returns(Right(judgement)).once()
    logger.info.expects(completedLog("success", prompts = 1, expectedToolCalls = 1)).once()

    val result = evaluator.evaluateCase(test).value

    result.correctToolCalls shouldBe AgentEvaluationResult.fromCorrectChooses(0, 1).value

  test("can evaluate a conversation and sum all tool calls"):
    val assistantBuilder = mock[Langchain4jAssistantBuilder]
    val assistant        = mock[Langchain4jAssistant]
    val judge            = mock[Langchain4jAgentEvaluationJudge]
    val logger           = mock[Logger]
    val firstToolCall: ToolCall.CreateCircleEntity =
      ToolCall.CreateCircleEntity(circleId, x, y, expectedRadius)
    val secondToolCall: ToolCall.CreateCircleEntity =
      ToolCall.CreateCircleEntity(secondCircleId, x, y, expectedRadius)
    val prompts   = Seq(prompt, secondPrompt)
    val test      = evaluationTest(Scene(), Seq(firstToolCall, secondToolCall), prompts)
    val judgement = successfulJudgement
    given Logger  = logger
    val evaluator = Langchain4jAgentEvaluator(assistantBuilder, Langchain4jToolCallMapper(), judge)

    assistantBuilder.build.expects(*, *).returns(assistant).once()
    assistant.chat
      .expects(conversationId, prompt)
      .returns(assistantResult(Seq(toolExecution(firstToolCall)), agentResponse))
      .once()
    assistant.chat
      .expects(conversationId, secondPrompt)
      .returns(assistantResult(Seq(toolExecution(secondToolCall)), secondAgentResponse))
      .once()
    judge.evaluate
      .expects(test, Seq(agentResponse, secondAgentResponse), *)
      .returns(Right(judgement))
      .once()
    logger.info.expects(completedLog("success", prompts = 2, expectedToolCalls = 2)).once()

    val result = evaluator.evaluateCase(test).value

    result.correctToolCalls shouldBe AgentEvaluationResult.fromCorrectChooses(2, 2).value

  test("returns an error when the assistant execution fails"):
    val assistantBuilder = mock[Langchain4jAssistantBuilder]
    val assistant        = mock[Langchain4jAssistant]
    val judge            = mock[Langchain4jAgentEvaluationJudge]
    val logger           = mock[Logger]
    val test             = evaluationTest(Scene(), Seq.empty)
    val errorMessage     = "model unavailable"

    assistantBuilder.build.expects(*, *).returns(assistant).once()
    assistant.chat.expects(conversationId, prompt).throws(new RuntimeException(errorMessage)).once()
    logger.info.expects(completedLog("failure", prompts = 1, expectedToolCalls = 0)).once()
    given Logger  = logger
    val evaluator = Langchain4jAgentEvaluator(assistantBuilder, Langchain4jToolCallMapper(), judge)

    val result = evaluator.evaluateCase(test)

    result shouldBe Left(AgentEvaluationExecutionError(errorMessage))

  test("can build a LangchainAgentEvaluator"):
    val agentConfig = Langchain4jOllamaConfig(
      url = "http://localhost:11434",
      modelName = "agent-model"
    )
    val judgeConfig = Langchain4jOllamaConfig(
      url = "http://localhost:11434",
      modelName = "judge-model"
    )
    val logger   = mock[Logger]
    given Logger = logger

    val result = Langchain4jAgentEvaluator.buildOllama(agentConfig, judgeConfig)

    result shouldBe a[Langchain4jAgentEvaluator]

  private def evaluationTest(
      initialScene: Scene,
      toolCalls: Seq[ToolCall],
      prompts: Seq[String] = Seq(prompt)
  ): AgentEvaluationTest =
    AgentEvaluationTest(
      initialScene = initialScene,
      prompts = prompts,
      language = AgentEvaluationLanguage.English,
      toolCalls = toolCalls,
      expectation = "The circle is created"
    )

  private def successfulJudgement: Langchain4jAgentEvaluationJudgement =
    Langchain4jAgentEvaluationJudgement(
      correctLanguageChoose = AgentEvaluationResult.Bool(true),
      languageCorrectness = AgentEvaluationResult.fromScore(80).value,
      expectationMaintained = AgentEvaluationResult.Bool(true)
    )

  private def toolExecution(toolCall: ToolCall.CreateCircleEntity): ToolExecution =
    val invocationContext = mock[InvocationContext]
    val request = ToolExecutionRequest
      .builder()
      .name("createCircleEntity")
      .arguments(
        s"""{"id":"${toolCall.id}","x":${toolCall.x},"y":${toolCall.y},"radius":${toolCall.radius}}"""
      )
      .build()

    ToolExecution
      .builder()
      .request(request)
      .result(
        ToolExecutionResult
          .builder()
          .resultText("Success")
          .build()
      )
      .invocationContext(invocationContext)
      .build()

  private def assistantResult(
      toolExecutions: Seq[ToolExecution],
      response: String = agentResponse
  ): Result[String] =
    Result
      .builder[String]()
      .content(response)
      .toolExecutions(toolExecutions.asJava)
      .build()

  private def completedLog(status: String, prompts: Int, expectedToolCalls: Int): String =
    s"event=agent_evaluation_test_completed status=$status prompts=$prompts expected_tool_calls=$expectedToolCalls"
