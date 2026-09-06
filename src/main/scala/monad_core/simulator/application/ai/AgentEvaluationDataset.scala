package monad_core.simulator.application.ai

import monad_core.simulator.domain.ai.agent_evaluation.AgentEvaluationTest

/** Dataset contains test cases used to evaluate an AI agent. */
trait AgentEvaluationDataset:
  /** @return evaluation cases */
  def tests: Seq[AgentEvaluationTest]
