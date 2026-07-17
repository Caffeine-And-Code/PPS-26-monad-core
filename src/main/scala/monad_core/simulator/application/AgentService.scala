package monad_core.simulator.application

import dev.langchain4j.model.chat.ChatModel
import dev.langchain4j.model.ollama.OllamaChatModel

case class ModelInfo(provider:String, model: String)

trait AgentService:
  def ask(message: String): String
  def modelInfo:ModelInfo

object AgentService:

  final case class OllamaConfig(
      baseUrl: String = "http://localhost:11434",
      modelName: String = "gemma4:e2b"
  )

  def apply(model: ChatModel): AgentService = new AgentService:
    override def ask(message: String): String =
      model.chat(message)
    override def modelInfo:ModelInfo =
      ModelInfo(model.provider.toString, model.defaultRequestParameters().modelName())

  def ollama(config: OllamaConfig = OllamaConfig()): AgentService =
    val model = OllamaChatModel.builder()
      .baseUrl(config.baseUrl)
      .modelName(config.modelName)
      .build()

    AgentService(model)

  given defaultAgentService: AgentService = ollama()
