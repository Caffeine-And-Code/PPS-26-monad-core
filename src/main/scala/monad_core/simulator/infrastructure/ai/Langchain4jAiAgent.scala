package monad_core.simulator.infrastructure.ai

import dev.langchain4j.memory.ChatMemory
import monad_core.simulator.application.ai.{AiAgent, AskAgentCommand, CleanHistoryCommand}
import monad_core.simulator.domain.ai.{
  AgentInfo,
  AgentResponse,
  AgentResponseError,
  ConversationNotFoundError
}
import monad_core.simulator.errors.BaseError

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}
import scala.util.Try

/**
 * [[monad_core.simulator.application.ai.AiAgent]] adapter based on LangChain4j AI service.
 *
 * @param assistant Langchain4j AI service
 * @param agentInfo provider and model info
 */
case class Langchain4jAiAgent(
    assistant: Langchain4jAssistant,
    agentInfo: AgentInfo
) extends AiAgent:

  override def getAgentInfo: AgentInfo = agentInfo

  override def ask(command: AskAgentCommand): Future[Either[AgentResponseError, AgentResponse]] =
    Future {
      blocking {
        Try(
          AgentResponse(assistant.chat(command.conversationId, command.prompt.toString).content())
        ).toEither.left
          .map(error => AgentResponseError(error.getMessage))
      }
    }

  override def cleanHistory(command: CleanHistoryCommand): Either[BaseError, Unit] =
    assistant.getChatMemory(command.conversationId) match
      case null                   => Left(ConversationNotFoundError(command.conversationId))
      case chatMemory: ChatMemory => Right(chatMemory.clear())
