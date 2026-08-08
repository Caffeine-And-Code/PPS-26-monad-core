package monad_core.simulator.infrastructure.ai

import dev.langchain4j.model.ollama.OllamaChatModel
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
    val model = buildOllamaModel(config)

    val assistant = Langchain4jAssistantFactory(model).build(word, gameEngineRuntime)

    val agentInfo = AgentInfo(
      provider = config.provider,
      modelName = config.modelName
    )

    Langchain4jAiAgent(
      assistant = assistant,
      agentInfo = agentInfo
    )

  private[ai] def buildOllamaModel(config: Langchain4jOllamaConfig): OllamaChatModel =
    OllamaChatModel.builder()
      .baseUrl(config.url)
      .modelName(config.modelName)
      .build()
