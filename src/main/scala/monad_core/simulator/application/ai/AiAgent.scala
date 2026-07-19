package monad_core.simulator.application.ai

import monad_core.simulator.domain.ai.{AgentResponse, AgentResponseError, ConversationId, UserPrompt}
import monad_core.simulator.errors.BaseError

case class AskAgentCommand(
                          conversationId: ConversationId,
                          prompt: UserPrompt,
                          )

case class CleanHistoryCommand(
                              conversationId: ConversationId
                              )

trait AiAgent :
  def ask(command: AskAgentCommand):Either[AgentResponseError, AgentResponse]

  def cleanHistory(command: CleanHistoryCommand): Either[BaseError, Unit]