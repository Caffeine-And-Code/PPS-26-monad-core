package monad_core.simulator.presentation.agent_evaluation

import monad_core.simulator.application.ai.{AgentEvaluationDataset, AgentEvaluator}
import monad_core.simulator.domain.ai.agent_evaluation.{AgentEvaluationRecap, AgentEvaluationTest}
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgentEvaluationRuntimeTest extends AnyFunSuite with Matchers with MockFactory:

  test("can evaluate dataset tests and print the result"):
    val evaluator       = mock[AgentEvaluator]
    val dataset         = mock[AgentEvaluationDataset]
    val printer         = mock[AgentEvaluatorPrinter]
    val tests           = Seq.empty[AgentEvaluationTest]
    val evaluationRecup = AgentEvaluationRecap(100, 90, 80, 70, 1)
    (() => dataset.tests).expects().returning(tests).once()
    evaluator.evaluate.expects(tests).returning(evaluationRecup).once()
    printer.print.expects(evaluationRecup).once()

    AgentEvaluationRuntime.handle()(using evaluator, dataset, printer)
