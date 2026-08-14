package monad_core.simulator.presentation.agent_evaluation

import monad_core.simulator.domain.ai.agent_evaluation.AgentEvaluationRecap
import monad_core.simulator.errors.BaseError

object AgentEvaluatorConsolePrinter extends AgentEvaluatorPrinter:

  override def print(recup: AgentEvaluationRecap): Unit =
    Console.println(
      s"""Agent evaluation results:
         |Correct language choice: ${recup.correctLanguageChoose}%
         |Language correctness: ${recup.languageCorrectness}%
         |Correct tool calls: ${recup.correctToolCalls}%
         |Expectation maintained: ${recup.expectationMaintained}%
         |Failed evaluations: ${recup.evaluationFailed}""".stripMargin
    )
