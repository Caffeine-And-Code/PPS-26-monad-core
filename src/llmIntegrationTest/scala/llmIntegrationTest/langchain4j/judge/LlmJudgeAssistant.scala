package llmIntegrationTest.langchain4j.judge

import dev.langchain4j.model.ollama.OllamaChatModel
import dev.langchain4j.service.{AiServices, SystemMessage, UserMessage, V}
import monad_core.simulator.infrastructure.ai.Langchain4jOllamaConfig

/** Binary outcome returned by an LLM-based test judge. */
enum Judgement:
  /** The evaluated response satisfies the supplied criterion. */
  case Pass

  /** The evaluated response does not satisfy the supplied criterion. */
  case Fail

/** Parses textual judge output into a [[Judgement]]. */
object Judgement:
  /**
   * Converts the judge response into a binary outcome.
   *
   * Leading and trailing whitespace and letter case are ignored. Any output other than `PASS` is treated as
   * [[Judgement.Fail]].
   *
   * @param response raw text returned by the judge model
   * @return [[Judgement.Pass]] only for `PASS`; [[Judgement.Fail]] otherwise
   */
  def from(response: String): Judgement =
    response.trim.toUpperCase match
      case "PASS" => Judgement.Pass
      case "FAIL" => Judgement.Fail
      case other  => Judgement.Fail

/**
 * LangChain4j AI service that evaluates an assistant response against a textual criterion.
 */
trait LlmJudgeAssistant:

  @SystemMessage(Array(
    "You are a test judge.",
    "Evaluate the assistant response exclusively against the supplied criterion.",
    "Reply with exactly PASS if the response satisfies the criterion.",
    "Otherwise reply with exactly FAIL.",
    "Do not add explanations or any other text."
  ))
  @UserMessage(Array(
    "Evaluation criterion:",
    "{{criterion}}",
    "",
    "Assistant response:",
    "<response>",
    "{{response}}",
    "</response>"
  ))
  /**
   * Evaluates an assistant response against one criterion.
   *
   * @param criterion condition that the candidate response must satisfy
   * @param response assistant response
   * @return raw judge output, expected to be either `PASS` or `FAIL`
   */
  def judge(
      @V("criterion") criterion: String,
      @V("response") response: String
  ): String

/** Builds [[LlmJudgeAssistant]] instances based on Ollama. */
object LlmJudgeAssistant:

  /**
   * Creates a deterministic judge service using an Ollama chat model.
   *
   * @param config Ollama endpoint and model configuration
   * @return LangChain4j object implementing [[LlmJudgeAssistant]]
   */
  def buildOllama(config: Langchain4jOllamaConfig): LlmJudgeAssistant =
    val model = OllamaChatModel.builder()
      .baseUrl(config.url)
      .modelName(config.modelName)
      .temperature(0.0)
      .build()

    AiServices.builder(classOf[LlmJudgeAssistant])
      .chatModel(model)
      .build()
