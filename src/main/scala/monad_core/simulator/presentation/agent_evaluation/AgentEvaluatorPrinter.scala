package monad_core.simulator.presentation.agent_evaluation

import monad_core.simulator.domain.ai.agent_evaluation.AgentEvaluationRecap
import monad_core.simulator.errors.BaseError

/** Trait for an agent evaluation result printer. */
trait AgentEvaluatorPrinter:

  /**
   * Prints an agent evaluation recup
   *
   * @param recup aggregate evaluation result to present
   */
  def print(recup: AgentEvaluationRecap): Unit
