package monad_core.simulator.domain.ai.agent_evaluation

case class AgentEvaluationResponse(
                                    correctLanguageChoose: AgentEvaluationResult.Bool,
                                    languageCorrectness: AgentEvaluationResult.Score,
                                    correctToolCalls: AgentEvaluationResult.CorrectChooses,
                                    expectationMaintained: AgentEvaluationResult.Bool
                                  )