package monad_core.simulator.domain.ai.agent_evaluation

case class AgentEvaluationRecup(
                                 correctLanguageChoose: Int,
                                 languageCorrectness: Int,
                                 correctToolCalls: Int,
                                 correctToolParams: Int,
                                 expectationMaintained: Int,
                                 evaluationFailed: Int
                               )