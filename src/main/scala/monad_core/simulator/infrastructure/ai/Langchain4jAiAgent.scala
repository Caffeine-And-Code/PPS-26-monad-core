package monad_core.simulator.infrastructure.ai

import monad_core.simulator.application.ai.{AiAgent, AskAgentCommand, CleanHistoryCommand}
import monad_core.simulator.domain.ai.{AgentInfo, AgentResponse, AgentResponseError}
import monad_core.simulator.errors.BaseError

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}
import scala.util.Try

case class Langchain4jAiAgent(
                             assistant: Langchain4jAssistant,
                             agentInfo: AgentInfo
                             )
extends AiAgent:
  override def ask(command: AskAgentCommand): Future[Either[AgentResponseError, AgentResponse]] =
    Future {
      blocking {
        Try(AgentResponse(assistant.chat(command.prompt.toString), 0))
          .toEither
          .left
          .map(error => AgentResponseError(error.getMessage))
      }
    }

  override def cleanHistory(command: CleanHistoryCommand): Either[BaseError, Unit] = ???

