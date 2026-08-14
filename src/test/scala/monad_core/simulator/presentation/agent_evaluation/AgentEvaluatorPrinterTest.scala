package monad_core.simulator.presentation.agent_evaluation

import monad_core.simulator.domain.ai.agent_evaluation.AgentEvaluationRecap
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgentEvaluatorPrinterTest extends AnyFunSuite with Matchers:

  test("can implement an AgentEvaluatorPrinter"):
    val evaluationRecup = AgentEvaluationRecap(100, 90, 80, 70, 1)
    var printedRecup    = Option.empty[AgentEvaluationRecap]
    val printer = new AgentEvaluatorPrinter:
      override def print(recup: AgentEvaluationRecap): Unit =
        printedRecup = Some(recup)

    printer.print(evaluationRecup)

    printedRecup shouldBe Some(evaluationRecup)
