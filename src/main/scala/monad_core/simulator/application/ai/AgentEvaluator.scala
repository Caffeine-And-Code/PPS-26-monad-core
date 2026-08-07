package monad_core.simulator.application.ai

import monad_core.simulator.domain.ai.agent_evaluation.{AgentEvaluationRecup, AgentEvaluationResponse, AgentEvaluationResult, AgentEvaluationTest}
import monad_core.simulator.errors.BaseError

trait AgentEvaluator:

  def evaluateCase(agentEvaluationTest: AgentEvaluationTest): Either[BaseError, AgentEvaluationResponse]

  def evaluate(tests: Seq[AgentEvaluationTest]): AgentEvaluationRecup =
    val responses = tests.map(evaluateCase)
    val validResponses = responses.flatMap(_.toOption)
    val invalidResponses = responses.filter(_.isLeft)
    AgentEvaluationRecup(
      correctLanguageChoose = getPercentualScore(validResponses.map(_.correctLanguageChoose)),
      languageCorrectness = getPercentualScore(validResponses.map(_.languageCorrectness)),
      correctToolCalls = getPercentualScore(validResponses.map(_.correctToolCalls)),
      correctToolParams = getPercentualScore(validResponses.map(_.correctToolParams)),
      expectationMaintained = getPercentualScore(validResponses.map(_.expectationMaintained)),
      evaluationFailed = invalidResponses.length
    )

  private def getPercentualScore(responses: Seq[AgentEvaluationResult]): Int = {
    if (responses.isEmpty) 0
    else responses
      .map(toScore)
      .sum / responses.length
  }

  private def toScore(value: AgentEvaluationResult): Int = value match
    case AgentEvaluationResult.Bool(result) => if (result) 100 else 0
    case AgentEvaluationResult.Score(result) => result
    case AgentEvaluationResult.CorrectChooses(correctChooses, on) =>
      if (on == 0) 0 else Math.round((correctChooses.toDouble / on) * 100).toInt
