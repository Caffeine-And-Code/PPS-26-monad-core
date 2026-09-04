package monad_core.simulator.domain.ai

import monad_core.simulator.errors.BaseError

/**
 * Text returned by an AI agent.
 *
 * @param response generated response text
 */
case class AgentResponse(
    response: String
)

/**
 * Error reported while obtaining an agent response.
 *
 * @param error error description
 */
case class AgentResponseError(error: String) extends BaseError(error)
