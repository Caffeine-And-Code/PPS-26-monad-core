package monad_core.simulator.domain.ai.agent_evaluation

import monad_core.simulator.errors.BaseError

type AgentEvaluationScore = Int

case class InvalidAgentEvaluationValue(value: Int)
    extends BaseError(s"value: $value is not a valid score result, it should be from 0 to 100")

object AgentEvaluationScore:

  def from(value: Int): Either[InvalidAgentEvaluationValue, AgentEvaluationScore] =
    Either.cond(value >= 0 && value <= 100, value, InvalidAgentEvaluationValue(value))

case class InvalidCorrectChooses(correctChooses: Int, on: Int)
    extends BaseError(
      s"correctChooses [$correctChooses] cannot be greater then on [$on], and they should be greater then 0"
    )

enum AgentEvaluationResult:
  case Bool(result: Boolean)
  case Score(result: AgentEvaluationScore)
  case CorrectChooses(correctChooses: Int, on: Int)

object AgentEvaluationResult:

  def fromBool(result: Boolean): AgentEvaluationResult.Bool =
    AgentEvaluationResult.Bool(result)

  def fromScore(score: Int): Either[InvalidAgentEvaluationValue, AgentEvaluationResult.Score] =
    for {
      value <- AgentEvaluationScore.from(score)
    } yield AgentEvaluationResult.Score(value)

  def fromCorrectChooses(
      correctChooses: Int,
      on: Int
  ): Either[InvalidCorrectChooses, AgentEvaluationResult.CorrectChooses] =
    Either.cond(
      correctChooses <= on && on >= 0,
      AgentEvaluationResult.CorrectChooses(correctChooses, on),
      InvalidCorrectChooses(correctChooses, on)
    )
