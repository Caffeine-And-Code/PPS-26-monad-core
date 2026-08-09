package monad_core.simulator.infrastructure.ai

import dev.langchain4j.memory.chat.MessageWindowChatMemory
import dev.langchain4j.model.ollama.OllamaChatModel
import dev.langchain4j.service.AiServices
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.domain.ai.AgentInfo

case class Langchain4jOllamaConfig (
                                   url: String,
                                   modelName: String,
                                   provider: String = "Ollama"
                                   )

trait Langchain4jAgentFactory:
  def buildOllama(config: Langchain4jOllamaConfig):Langchain4jAiAgent

object Langchain4jAgentFactory :

  def buildOllama(config: Langchain4jOllamaConfig)(
    using word: World,
    gameEngineRuntime: GameEngineRuntime
  ): Langchain4jAiAgent =
    val model = OllamaChatModel.builder()
      .baseUrl(config.url)
      .modelName(config.modelName)
      .build()

    val assistant = AiServices.builder(classOf[Langchain4jAssistant])
      .chatModel(model)
      .chatMemoryProvider(_ => MessageWindowChatMemory.withMaxMessages(10))
      .tools(Langchain4jTools())
      .build()

    val agentInfo = AgentInfo(
      provider = config.provider,
      modelName = config.modelName
    )

    Langchain4jAiAgent(
      assistant = assistant,
      agentInfo = agentInfo
    )
