package monad_core.simulator.application.ai

import monad_core.simulator.domain.ai.{AgentResponse, AgentResponseError, ConversationId, UserPrompt}

case class AskAgentCommand(
                          conversationId: ConversationId,
                          prompt: UserPrompt,
                          )

trait AiAgent :
  def ask(command: AskAgentCommand):Either[AgentResponseError, AgentResponse]
