package monad_core.simulator.application

import monad_core.simulator.errors.BaseError

case class AgentCallError(toolResponse:String) extends BaseError(toolResponse) 