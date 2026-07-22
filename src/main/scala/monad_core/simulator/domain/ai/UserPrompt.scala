package monad_core.simulator.domain.ai

import monad_core.simulator.errors.BaseError

opaque type UserPrompt = String

case class InvalidUserPrompt() extends BaseError("Invalid user prompt")

object UserPrompt:

  def from(raw: String): Either[InvalidUserPrompt, UserPrompt] =
    Either.cond(raw.trim.nonEmpty, raw, InvalidUserPrompt())