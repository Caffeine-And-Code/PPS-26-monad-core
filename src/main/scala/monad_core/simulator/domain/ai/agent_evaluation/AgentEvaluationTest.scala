package monad_core.simulator.domain.ai.agent_evaluation

import monad_core.engine.model.Scene

/** Supported Language expected from an agent during an evaluation case. */
enum AgentEvaluationLanguage:
  /** Italian responses are expected. */
  case Italian

  /** English responses are expected. */
  case English

/**
 * Input for one agent evaluation test case.
 *
 * @param initialScene scene at the beginning of the conversation
 * @param prompts user prompts in conversation order
 * @param language expected response language
 * @param toolCalls expected tool calls in execution order
 * @param expectation natural language description of the expected result
 */
case class AgentEvaluationTest(
    initialScene: Scene,
    prompts: Seq[String],
    language: AgentEvaluationLanguage,
    toolCalls: Seq[ToolCall],
    expectation: String
)
