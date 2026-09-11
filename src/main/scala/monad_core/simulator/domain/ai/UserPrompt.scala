package monad_core.simulator.domain.ai

import monad_core.simulator.errors.BaseError

/** Not empty text submitted by a user to an AI agent. */
opaque type UserPrompt = String

/** Error returned when a blank prompt is supplied. */
case class InvalidUserPrompt() extends BaseError("Invalid user prompt")

/** Factory for [[UserPrompt]]. */
object UserPrompt:

  /**
   * Creates a user prompt.
   *
   * @param raw raw prompt text
   * @return the prompt when not empty, or [[InvalidUserPrompt]]
   */
  def from(raw: String): Either[InvalidUserPrompt, UserPrompt] =
    Either.cond(raw.trim.nonEmpty, raw, InvalidUserPrompt())
