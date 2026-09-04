package monad_core.simulator.presentation.agent_evaluation

import monad_core.simulator.application.ai.{AgentEvaluationDataset, AgentEvaluator}

/** Runs the configured evaluation dataset and presents its aggregate result. */
object AgentEvaluationRuntime:

  /**
   * Evaluates all dataset cases and sends the recap to the configured printer.
   *
   * @param agentEvaluator evaluator used for each case
   * @param agentEvaluatorDataset source of evaluation cases
   * @param agentEvaluatorPrinter output printer for the recap
   */
  def handle()(using
      agentEvaluator: AgentEvaluator,
      agentEvaluatorDataset: AgentEvaluationDataset,
      agentEvaluatorPrinter: AgentEvaluatorPrinter
  ): Unit =
    agentEvaluatorPrinter.print(
      agentEvaluator.evaluate(
        agentEvaluatorDataset.tests
      )
    )
