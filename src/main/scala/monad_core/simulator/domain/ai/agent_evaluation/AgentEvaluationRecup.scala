package monad_core.simulator.domain.ai.agent_evaluation

case class AgentEvaluationRecup(
    correctLanguageChoose: Int,
    languageCorrectness: Int,
    correctToolCalls: Int,
    expectationMaintained: Int,
    evaluationFailed: Int
)
