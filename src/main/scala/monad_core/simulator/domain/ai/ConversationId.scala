package monad_core.simulator.domain.ai

import monad_core.simulator.errors.BaseError

opaque type ConversationId = String

case class InvalidConversationId() extends BaseError("Invalid conversation Id")

object ConversationId:

  def from(conversationId: String): Either[InvalidConversationId, ConversationId] =
    Either.cond(conversationId.trim.nonEmpty, conversationId, InvalidConversationId())