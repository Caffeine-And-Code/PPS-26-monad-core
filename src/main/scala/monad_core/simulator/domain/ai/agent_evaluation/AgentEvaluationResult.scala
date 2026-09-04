package monad_core.simulator.domain.ai.agent_evaluation

import monad_core.simulator.errors.BaseError

/** Integer evaluation score in the inclusive range `[0, 100]`. */
opaque type AgentEvaluationScore = Int

/** @param value integer outside the valid evaluation score range */
case class InvalidAgentEvaluationValue(value: Int)
    extends BaseError(s"value: $value is not a valid score result, it should be from 0 to 100")

/** Factory and operations for [[AgentEvaluationScore]]. */
object AgentEvaluationScore:

  /**
   * @param value raw score
   * @return validated score, or [[InvalidAgentEvaluationValue]]
   */
  def from(value: Int): Either[InvalidAgentEvaluationValue, AgentEvaluationScore] =
    Either.cond(value >= 0 && value <= 100, value, InvalidAgentEvaluationValue(value))

  extension (score: AgentEvaluationScore)
    /** @return integer represented by the score */
    def value: Int = score

/**
 * @param correctChooses number of matching choices
 * @param on total number of choices
 */
case class InvalidCorrectChooses(correctChooses: Int, on: Int)
    extends BaseError(
      s"correctChooses [$correctChooses] must be between 0 and on [$on]"
    )

/** Validated measurement produced while evaluating an agent. */
sealed trait AgentEvaluationResult

/** Constructors and result variants for [[AgentEvaluationResult]]. */
object AgentEvaluationResult:

  /** @param result boolean measurement */
  final case class Bool(result: Boolean) extends AgentEvaluationResult

  /** @param result score measurement */
  final case class Score private (result: AgentEvaluationScore) extends AgentEvaluationResult

  /** @param correctChooses number of matching choices @param on total number of choices */
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

  /**
   * Creates a boolean agent evaluation result
   *
   * @param result boolean value
   * @return boolean evaluation result
   */
  def fromBool(result: Boolean): AgentEvaluationResult.Bool =
    AgentEvaluationResult.Bool(result)

  /**
   * Creates a score agent evaluation result
   *
   * @param score raw score
   * @return score result, or an error [[InvalidAgentEvaluationValue]] when the score is not in range [0, 100]
   */
  def fromScore(score: Int): Either[InvalidAgentEvaluationValue, AgentEvaluationResult.Score] =
    for {
      value <- AgentEvaluationScore.from(score)
    } yield AgentEvaluationResult.Score.validated(value)

  /**
   * Creates a correct choice count agent evaluation result.
   *
   * @param correctChooses number of matching choices
   * @param on non-negative total number of choices
   * @return validated result, or [[InvalidCorrectChooses]] when the count is outside `[0, on]`
   */
  def fromCorrectChooses(
      correctChooses: Int,
      on: Int
  ): Either[InvalidCorrectChooses, AgentEvaluationResult.CorrectChooses] =
    Either.cond(
      correctChooses >= 0 && correctChooses <= on && on >= 0,
      AgentEvaluationResult.CorrectChooses.validated(correctChooses, on),
      InvalidCorrectChooses(correctChooses, on)
    )
