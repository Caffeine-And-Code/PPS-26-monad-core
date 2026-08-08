package monad_core.simulator.application.ai

import monad_core.simulator.domain.ai.agent_evaluation.AgentEvaluationTest

trait AgentEvaluationDataset:
  def tests: Seq[AgentEvaluationTest]
