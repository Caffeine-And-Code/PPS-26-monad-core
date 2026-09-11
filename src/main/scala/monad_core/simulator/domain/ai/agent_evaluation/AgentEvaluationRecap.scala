package monad_core.simulator.domain.ai.agent_evaluation

/**
 * Measurement computed across an evaluation dataset.
 *
 * @param correctLanguageChoose percentage of cases using the expected language
 * @param languageCorrectness average language quality score
 * @param correctToolCalls percentage of expected tool calls selected correctly
 * @param expectationMaintained percentage of cases satisfying the expected outcome
 * @param evaluationFailed number of cases that could not be evaluated
 */
case class AgentEvaluationRecap(
    correctLanguageChoose: Int,
    languageCorrectness: Int,
    correctToolCalls: Int,
    expectationMaintained: Int,
    evaluationFailed: Int
)
