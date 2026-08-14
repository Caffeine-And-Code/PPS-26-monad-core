package monad_core.simulator.infrastructure.ai.agent_evaluator

import dev.langchain4j.model.chat.ChatModel
import dev.langchain4j.service.AiServices
import dev.langchain4j.service.{SystemMessage, UserMessage, V}
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.domain.ai.agent_evaluation.{AgentEvaluationResult, AgentEvaluationTest}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.ai.Langchain4jToolResponse.{
  renderEntity,
  renderSurface,
  renderTeam
}

import scala.beans.BeanProperty
import scala.util.Try

class Langchain4jAgentEvaluationResult:
  @BeanProperty var correctLanguageChoose: Boolean = false
  @BeanProperty var languageCorrectness: Int       = 0
  @BeanProperty var expectationMaintained: Boolean = false

trait Langchain4jAgentEvaluationJudgeAssistant:

  @SystemMessage(
    Array(
      "You evaluate an AI assistant for the MonadCore2D scene editor.",
      "correctLanguageChoose is true only when the response uses the expected language.",
      "languageCorrectness measures grammar and clarity in the expected language (is an integer between 0 and 100).",
      "expectationMaintained is true only when the response and final world satisfy the stated expectation."
    )
  )
  @UserMessage(
    Array(
      "Expected language: {{expectedLanguage}}",
      "User prompts in conversation order:",
      "{{userPrompts}}",
      "Expected outcome: {{expectedOutcome}}",
      "Assistant responses in conversation order:",
      "{{assistantResponses}}",
      "Final world entities:",
      "{{entities}}",
      "Final world surfaces:",
      "{{surfaces}}",
      "Final world teams:",
      "{{teams}}"
    )
  )
  def evaluate(
      @V("expectedLanguage") expectedLanguage: String,
      @V("userPrompts") userPrompts: String,
      @V("expectedOutcome") expectedOutcome: String,
      @V("assistantResponses") assistantResponses: String,
      @V("entities") entities: String,
      @V("surfaces") surfaces: String,
      @V("teams") teams: String
  ): Langchain4jAgentEvaluationResult

case class InvalidAgentEvaluationJudgement(reason: String)
    extends BaseError(s"Invalid agent evaluation judgement: $reason")

case class Langchain4jAgentEvaluationJudgement(
    correctLanguageChoose: AgentEvaluationResult.Bool,
    languageCorrectness: AgentEvaluationResult.Score,
    expectationMaintained: AgentEvaluationResult.Bool
)

case class Langchain4jAgentEvaluationJudge(
    assistant: Langchain4jAgentEvaluationJudgeAssistant
):

  def evaluate(
      test: AgentEvaluationTest,
      agentResponses: Seq[String],
      finalWorld: World
  ): Either[BaseError, Langchain4jAgentEvaluationJudgement] =
    for
      judgement <- Try {
        assistant.evaluate(
          expectedLanguage = test.language.toString,
          userPrompts = renderMessages(test.prompts),
          expectedOutcome = test.expectation,
          assistantResponses = renderMessages(agentResponses),
          entities = render(finalWorld.getAllEntities)(renderEntity),
          surfaces = render(finalWorld.getAllSurfaces)(renderSurface),
          teams = render(finalWorld.getAllTeams)(renderTeam)
        )
      }.toEither.left.map(error => InvalidAgentEvaluationJudgement(error.getMessage))
      languageScore <- AgentEvaluationResult.fromScore(judgement.languageCorrectness)
    yield Langchain4jAgentEvaluationJudgement(
      correctLanguageChoose = AgentEvaluationResult.fromBool(judgement.correctLanguageChoose),
      languageCorrectness = languageScore,
      expectationMaintained = AgentEvaluationResult.fromBool(judgement.expectationMaintained)
    )

  private def render[A](values: List[A])(renderer: A => String): String =
    if values.isEmpty then "none"
    else values.map(renderer).mkString("\n---\n")

  private def renderMessages(messages: Seq[String]): String =
    if messages.isEmpty then "none"
    else
      messages.zipWithIndex
        .map: (message, index) =>
          s"${index + 1}: $message"
        .mkString("\n")

object Langchain4jAgentEvaluationJudge:

  def build(chatModel: ChatModel): Langchain4jAgentEvaluationJudge =
    val assistant = AiServices
      .builder(classOf[Langchain4jAgentEvaluationJudgeAssistant])
      .chatModel(chatModel)
      .build()

    Langchain4jAgentEvaluationJudge(assistant)
