package monad_core.simulator.infrastructure.ai.agent_evaluator

import dev.langchain4j.service.tool.ToolExecution
import monad_core.simulator.application.ai.AgentEvaluator
import monad_core.simulator.application.logging.Logger
import monad_core.simulator.domain.ai.ConversationId
import monad_core.simulator.domain.ai.agent_evaluation.{
  AgentEvaluationResponse,
  AgentEvaluationResult,
  AgentEvaluationTest,
  ToolCall
}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.ai.{
  Langchain4jAgentFactory,
  Langchain4jAssistantBuilder,
  Langchain4jAssistantFactory,
  Langchain4jOllamaConfig
}
import monad_core.simulator.infrastructure.engine.{HeadlessEngineControl, MonadCoreWorld}

import scala.jdk.CollectionConverters.*
import scala.util.Try

case class AgentEvaluationExecutionError(reason: String)
    extends BaseError(s"Agent evaluation failed: $reason")

case class Langchain4jAgentEvaluator(
    assistantBuilder: Langchain4jAssistantBuilder,
    toolCallMapper: Langchain4jToolCallMapper,
    evaluationJudge: Langchain4jAgentEvaluationJudge
)(using logger: Logger)
    extends AgentEvaluator:

  override def evaluateCase(
      test: AgentEvaluationTest
  ): Either[BaseError, AgentEvaluationResponse] = {
    val result = Try {
      val evaluationWorld = MonadCoreWorld(test.initialScene)
      val engineControl   = HeadlessEngineControl()
      val assistant       = assistantBuilder.build(evaluationWorld, engineControl)
      val results = test.prompts.map: prompt =>
        assistant.chat(evaluationConversationId, prompt)

      for
        actualToolCalls <- mapToolCalls(results.flatMap(_.toolExecutions().asScala))
        judgement       <- evaluationJudge.evaluate(test, results.map(_.content()), evaluationWorld)
      yield AgentEvaluationResponse(
        correctLanguageChoose = judgement.correctLanguageChoose,
        languageCorrectness = judgement.languageCorrectness,
        correctToolCalls = correctToolCalls(test.toolCalls, actualToolCalls),
        expectationMaintained = judgement.expectationMaintained
      )
    }.toEither.left
      .map(error => AgentEvaluationExecutionError(error.getMessage))
      .flatMap(result => result)

    logger.info(evaluationCompletedLog(test, result))
    result
  }

  private val evaluationConversationId: ConversationId =
    ConversationId.from("agent-evaluation").toOption.get

  private def mapToolCalls(executions: Seq[ToolExecution]): Either[BaseError, Seq[ToolCall]] =
    executions.foldLeft(Right(Seq.empty): Either[BaseError, Seq[ToolCall]]): (result, execution) =>
      for
        mappedCalls <- result
        toolCall    <- toolCallMapper.from(execution.request())
      yield mappedCalls :+ toolCall

  private def correctToolCalls(
      expected: Seq[ToolCall],
      actual: Seq[ToolCall]
  ): AgentEvaluationResult.CorrectChooses =
    val correct = expected
      .zip(actual)
      .count: (expectedCall, actualCall) =>
        expectedCall.productPrefix == actualCall.productPrefix

    AgentEvaluationResult.CorrectChooses(correct, Math.max(expected.length, actual.length))

  private def evaluationCompletedLog(
      test: AgentEvaluationTest,
      result: Either[BaseError, AgentEvaluationResponse]
  ): String =
    val status = if result.isRight then "success" else "failure"
    s"event=agent_evaluation_test_completed status=$status prompts=${test.prompts.length} expected_tool_calls=${test.toolCalls.length}"

object Langchain4jAgentEvaluator:

  def buildOllama(
      agentConfig: Langchain4jOllamaConfig,
      judgeConfig: Langchain4jOllamaConfig
  )(using Logger): Langchain4jAgentEvaluator =
    val agentModel = Langchain4jAgentFactory.buildOllamaModel(agentConfig)
    val judgeModel = Langchain4jAgentFactory.buildOllamaModel(judgeConfig)

    Langchain4jAgentEvaluator(
      assistantBuilder = Langchain4jAssistantFactory(agentModel),
      toolCallMapper = Langchain4jToolCallMapper(),
      evaluationJudge = Langchain4jAgentEvaluationJudge.build(judgeModel)
    )
