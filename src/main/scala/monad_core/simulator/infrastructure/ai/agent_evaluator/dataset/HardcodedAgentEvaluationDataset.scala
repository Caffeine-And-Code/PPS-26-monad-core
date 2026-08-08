package monad_core.simulator.infrastructure.ai.agent_evaluator.dataset

import monad_core.simulator.application.ai.AgentEvaluationDataset
import monad_core.simulator.domain.ai.agent_evaluation.AgentEvaluationTest

object HardcodedAgentEvaluationDataset extends AgentEvaluationDataset:
  override val tests: Seq[AgentEvaluationTest] = Seq.empty
