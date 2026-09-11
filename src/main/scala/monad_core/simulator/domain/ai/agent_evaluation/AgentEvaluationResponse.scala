package monad_core.simulator.domain.ai.agent_evaluation

/**
 * Measurements produced for one agent evaluation case.
 *
 * @param correctLanguageChoose whether the expected language was used by the AI agent
 * @param languageCorrectness validated language quality score
 * @param correctToolCalls validated ratio of matching tool calls
 * @param expectationMaintained whether the expected final outcome was satisfied
 */
case class AgentEvaluationResponse(
    correctLanguageChoose: AgentEvaluationResult.Bool,
    languageCorrectness: AgentEvaluationResult.Score,
    correctToolCalls: AgentEvaluationResult.CorrectChooses,
    expectationMaintained: AgentEvaluationResult.Bool
)
