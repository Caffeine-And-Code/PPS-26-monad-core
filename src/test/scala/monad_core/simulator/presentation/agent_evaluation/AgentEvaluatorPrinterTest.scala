package monad_core.simulator.presentation.agent_evaluation

import monad_core.simulator.domain.ai.agent_evaluation.AgentEvaluationRecup
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgentEvaluatorPrinterTest extends AnyFunSuite with Matchers:

  test("can implement an AgentEvaluatorPrinter"):
    val evaluationRecup = AgentEvaluationRecup(100, 90, 80, 70, 1)
    var printedRecup    = Option.empty[AgentEvaluationRecup]
    val printer = new AgentEvaluatorPrinter:
      override def print(recup: AgentEvaluationRecup): Unit =
        printedRecup = Some(recup)

    printer.print(evaluationRecup)

    printedRecup shouldBe Some(evaluationRecup)
