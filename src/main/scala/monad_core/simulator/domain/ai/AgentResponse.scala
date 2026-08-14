package monad_core.simulator.domain.ai

import monad_core.simulator.errors.BaseError

case class AgentResponse(
    response: String
)

case class AgentResponseError(error: String) extends BaseError(error)
