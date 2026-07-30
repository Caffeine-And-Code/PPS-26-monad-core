package monad_core.langchain4j.judge

import dev.langchain4j.model.ollama.OllamaChatModel
import dev.langchain4j.service.{AiServices, SystemMessage, UserMessage, V}
import monad_core.simulator.infrastructure.ai.Langchain4jOllamaConfig

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
  def judge(
      @V("criterion") criterion: String,
      @V("response") response: String
  ): String

object LlmJudgeAssistant:

  def buildOllama(config: Langchain4jOllamaConfig): LlmJudgeAssistant =
    val model = OllamaChatModel.builder()
      .baseUrl(config.url)
      .modelName(config.modelName)
      .temperature(0.0)
      .build()

    AiServices.builder(classOf[LlmJudgeAssistant])
      .chatModel(model)
      .build()
