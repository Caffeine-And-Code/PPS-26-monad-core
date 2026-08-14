package monad_core.simulator.presentation.agent_evaluation

import monad_core.simulator.domain.ai.agent_evaluation.AgentEvaluationRecap
import monad_core.simulator.errors.BaseError

trait AgentEvaluatorPrinter:
  def print(recup: AgentEvaluationRecap): Unit
