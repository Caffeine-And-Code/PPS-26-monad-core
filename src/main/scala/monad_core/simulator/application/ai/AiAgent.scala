package monad_core.simulator.application.ai

import monad_core.simulator.domain.ai.*
import monad_core.simulator.errors.BaseError

import scala.concurrent.Future

/**
 * Request to send a prompt within a conversation.
 *
 * @param conversationId identifier of the conversation
 * @param prompt user prompt
 */
case class AskAgentCommand(
    conversationId: ConversationId,
    prompt: UserPrompt
)

/**
 * Request to clear a conversation.
 *
 * @param conversationId identifier of the conversation to clear
 */
case class CleanHistoryCommand(
    conversationId: ConversationId
)

/** Provides access to an AI agent. */
trait AiAgent:
  /**
   * Sends a prompt to the agent.
   *
   * @param command conversation and prompt to process
   * @return asynchronous response, or an agent error
   */
  def ask(command: AskAgentCommand): Future[Either[AgentResponseError, AgentResponse]]

  /** @return provider and model information for this agent */
  def getAgentInfo: AgentInfo

  /**
   * Clears a conversation history.
   *
   * @param command conversation to clear
   * @return success, or an error when the conversation cannot be cleared
   */
  def cleanHistory(command: CleanHistoryCommand): Either[BaseError, Unit]
