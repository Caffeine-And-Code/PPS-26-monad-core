package monad_core.simulator.application.ai

import monad_core.simulator.domain.ai.*
import monad_core.simulator.errors.BaseError

import scala.concurrent.Future

case class AskAgentCommand(
                          conversationId: ConversationId,
                          prompt: UserPrompt,
                          )

case class CleanHistoryCommand(
                              conversationId: ConversationId
                              )

trait AiAgent :
  def ask(command: AskAgentCommand): Future[Either[AgentResponseError, AgentResponse]]

  def getAgentInfo: AgentInfo

  def cleanHistory(command: CleanHistoryCommand): Either[BaseError, Unit]