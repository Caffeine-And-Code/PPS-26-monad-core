package monad_core.simulator.domain.ai

import monad_core.simulator.errors.BaseError

case class ConversationNotFoundError(conversationId: ConversationId)
    extends BaseError(s"Conversation $conversationId not found")
