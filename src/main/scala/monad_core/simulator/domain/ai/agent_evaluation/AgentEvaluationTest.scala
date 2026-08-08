package monad_core.simulator.domain.ai.agent_evaluation

import monad_core.engine.core.Scene

enum AgentEvaluationLanguage:
  case Italian
  case English

case class AgentEvaluationTest(
                                initialScene: Scene,
                                prompts: Seq[String],
                                language: AgentEvaluationLanguage,
                                toolCalls: Seq[ToolCall],
                                expectation: String
                              )
