package monad_core.simulator.presentation.agent_evaluation

import monad_core.simulator.domain.ai.agent_evaluation.AgentEvaluationRecup
import monad_core.simulator.errors.BaseError

trait AgentEvaluatorPrinter:
  def print(recup: AgentEvaluationRecup): Unit
