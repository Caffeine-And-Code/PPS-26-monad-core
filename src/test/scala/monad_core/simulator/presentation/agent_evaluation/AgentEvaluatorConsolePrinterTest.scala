package monad_core.simulator.presentation.agent_evaluation

import monad_core.simulator.domain.ai.agent_evaluation.AgentEvaluationRecup
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

class AgentEvaluatorConsolePrinterTest extends AnyFunSuite with Matchers:

  test("can print an AgentEvaluationRecup"):
    val evaluationRecup = AgentEvaluationRecup(100, 90, 80, 70, 1)
    val output          = ByteArrayOutputStream()
    val expectedOutput =
      s"""Agent evaluation results:
         |Correct language choice: ${evaluationRecup.correctLanguageChoose}%
         |Language correctness: ${evaluationRecup.languageCorrectness}%
         |Correct tool calls: ${evaluationRecup.correctToolCalls}%
         |Expectation maintained: ${evaluationRecup.expectationMaintained}%
         |Failed evaluations: ${evaluationRecup.evaluationFailed}
         |""".stripMargin

    Console.withOut(output):
      AgentEvaluatorConsolePrinter.print(evaluationRecup)

    output.toString(StandardCharsets.UTF_8) shouldBe expectedOutput
