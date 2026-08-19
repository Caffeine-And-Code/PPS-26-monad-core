package monad_core.simulator.domain.ai.agent_evaluation

import monad_core.simulator.errors.BaseError

opaque type AgentEvaluationScore = Int

case class InvalidAgentEvaluationValue(value: Int)
    extends BaseError(s"value: $value is not a valid score result, it should be from 0 to 100")

object AgentEvaluationScore:

  def from(value: Int): Either[InvalidAgentEvaluationValue, AgentEvaluationScore] =
    Either.cond(value >= 0 && value <= 100, value, InvalidAgentEvaluationValue(value))

  extension (score: AgentEvaluationScore) def value: Int = score

case class InvalidCorrectChooses(correctChooses: Int, on: Int)
    extends BaseError(
      s"correctChooses [$correctChooses] must be between 0 and on [$on]"
    )

sealed trait AgentEvaluationResult

object AgentEvaluationResult:

  final case class Bool(result: Boolean)                        extends AgentEvaluationResult
  final case class Score private (result: AgentEvaluationScore) extends AgentEvaluationResult

  final case class CorrectChooses private (correctChooses: Int, on: Int)
      extends AgentEvaluationResult

  object Score:

    private[AgentEvaluationResult] def validated(
        result: AgentEvaluationScore
    ): AgentEvaluationResult.Score =
      new AgentEvaluationResult.Score(result)

  object CorrectChooses:

    private[AgentEvaluationResult] def validated(
        correctChooses: Int,
        on: Int
    ): AgentEvaluationResult.CorrectChooses =
      new AgentEvaluationResult.CorrectChooses(correctChooses, on)

  def fromBool(result: Boolean): AgentEvaluationResult.Bool =
    AgentEvaluationResult.Bool(result)

  def fromScore(score: Int): Either[InvalidAgentEvaluationValue, AgentEvaluationResult.Score] =
    for {
      value <- AgentEvaluationScore.from(score)
    } yield AgentEvaluationResult.Score.validated(value)

  def fromCorrectChooses(
      correctChooses: Int,
      on: Int
  ): Either[InvalidCorrectChooses, AgentEvaluationResult.CorrectChooses] =
    Either.cond(
      correctChooses >= 0 && correctChooses <= on && on >= 0,
      AgentEvaluationResult.CorrectChooses.validated(correctChooses, on),
      InvalidCorrectChooses(correctChooses, on)
    )
