package monad_core.simulator.presentation.agent_evaluation

import monad_core.simulator.application.ai.{AgentEvaluationDataset, AgentEvaluator}

object AgentEvaluationRuntime:

  def handle()
            (using agentEvaluator: AgentEvaluator,
             agentEvaluatorDataset: AgentEvaluationDataset,
             agentEvaluatorPrinter: AgentEvaluatorPrinter
            ): Unit = {
    agentEvaluatorPrinter.print(
      agentEvaluator.evaluate(
        agentEvaluatorDataset.tests
      )
    )
  }