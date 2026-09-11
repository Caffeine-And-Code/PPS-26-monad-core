package monad_core.simulator.domain.ai

import monad_core.simulator.errors.BaseError

/** Not empty identifier used to identify an agent conversation history. */
opaque type ConversationId = String

/** Error returned when a blank conversation identifier is supplied. */
case class InvalidConversationId() extends BaseError("Invalid conversation Id")

/** Factory for [[ConversationId]]. */
object ConversationId:

  /**
   * Creates a conversation identifier.
   *
   * @param conversationId raw identifier
   * @return the identifier when not empty, or [[InvalidConversationId]]
   */
  def from(conversationId: String): Either[InvalidConversationId, ConversationId] =
    Either.cond(conversationId.trim.nonEmpty, conversationId, InvalidConversationId())
