package monad_core.simulator.domain.ai.agent_evaluator

import monad_core.simulator.domain.ai.agent_evaluation.{
  AgentEvaluationResult,
  AgentEvaluationScore,
  InvalidAgentEvaluationValue
}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside.inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.*

class AgentEvaluationResultTest extends AnyFunSuite with Matchers:

  test("can create an AgentEvaluationScore with a value from 0 to 100"):
    val cases = Table(
      "value",
      0,
      50,
      100
    )

    forAll(cases): value =>
      AgentEvaluationScore.from(value) shouldBe Right(value)

  test("creating an AgentEvaluationScore with an invalid value got Error"):
    val cases = Table(
      "value",
      -1,
      101,
      200
    )

    forAll(cases): value =>
      AgentEvaluationScore.from(value) shouldBe Left(InvalidAgentEvaluationValue(value))

  test("can create a Bool AgentEvaluationResult"):
    val expected = true

    val result = AgentEvaluationResult.fromBool(expected)

    inside(result):
      case AgentEvaluationResult.Bool(res) => res shouldBe expected

  test("can create a Score AgentEvaluationResult"):
    val expected = AgentEvaluationScore.from(70).value

    val result = AgentEvaluationResult.fromScore(expected)

    inside(result.value):
      case AgentEvaluationResult.Score(res) => res shouldBe expected

  test("can create a CorrectChooses AgentEvaluationResult"):
    val correctChooses = 4
    val on             = 5

    val result = AgentEvaluationResult.fromCorrectChooses(correctChooses, on)

    inside(result.value):
      case AgentEvaluationResult.CorrectChooses(resCorrectChooses, resOn) =>
        resCorrectChooses shouldBe correctChooses
        resOn shouldBe on
