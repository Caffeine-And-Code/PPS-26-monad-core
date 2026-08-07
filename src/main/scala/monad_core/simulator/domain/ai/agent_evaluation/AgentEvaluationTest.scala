package monad_core.simulator.domain.ai.agent_evaluation

import monad_core.simulator.application.engine.world.World

enum AgentEvaluationLanguage:
  case Italian
  case English

case class AgentEvaluationTest(
                                initialWorld: World,
                                prompt: String,
                                language: AgentEvaluationLanguage,
                                toolCalls: Seq[ToolCall],
                                expectation: String
                              )
