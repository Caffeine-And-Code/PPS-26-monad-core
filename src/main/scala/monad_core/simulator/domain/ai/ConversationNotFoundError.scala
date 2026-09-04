package monad_core.simulator.domain.ai

import monad_core.simulator.errors.BaseError

/**
 * Error returned when the supplied conversation not exists.
 *
 * @param conversationId identifier that could not be found
 */
case class ConversationNotFoundError(conversationId: ConversationId)
    extends BaseError(s"Conversation $conversationId not found")
