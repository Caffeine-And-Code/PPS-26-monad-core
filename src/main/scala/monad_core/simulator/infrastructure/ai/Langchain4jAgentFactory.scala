package monad_core.simulator.infrastructure.ai

import dev.langchain4j.model.ollama.OllamaChatModel
import monad_core.simulator.application.ai.AiAgent
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.domain.ai.AgentInfo

/**
 * Langchain4j required params for using Ollama
 *
 * @param url Ollama server URL @param modelName Ollama model name
 * @param provider provider label exposed by the agent
 */
case class Langchain4jOllamaConfig(
    url: String,
    modelName: String,
    provider: String = "Ollama"
)

/** Factory for building [[AiAgent]] using Langchain4j. */
trait Langchain4jAgentFactory:

  /**
   * Build an [[AiAgent]] using Langhchain4j with an LLM served by Ollama
   *
   * @param config Ollama connection settings
   * @return agent connected to the configured model
   */
  def buildOllama(config: Langchain4jOllamaConfig): Langchain4jAiAgent

/** Default [[Langchain4jAgentFactory]] implementation. */
object Langchain4jAgentFactory:

  /**
   * Builds an AI agent based on Ollama with the ability to interact with the Word and with the game engine runtime.
   *
   * @param config Ollama connection settings
   * @param word world exposed to the agent tools
   * @param gameEngineRuntime runtime controlled by the agent tools
   * @return configured agent
   */
  def buildOllama(config: Langchain4jOllamaConfig)(using
      word: World,
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
    OllamaChatModel
      .builder()
      .baseUrl(config.url)
      .modelName(config.modelName)
      .build()
