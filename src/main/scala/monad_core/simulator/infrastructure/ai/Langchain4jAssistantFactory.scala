package monad_core.simulator.infrastructure.ai

import dev.langchain4j.memory.chat.MessageWindowChatMemory
import dev.langchain4j.model.chat.ChatModel
import dev.langchain4j.service.AiServices
import monad_core.simulator.application.engine.EngineControl
import monad_core.simulator.application.engine.world.World

/** Builds assistants with world and game engine interaction tools. */
trait Langchain4jAssistantBuilder:

  /**
   * Create the AI agent with world and game engine tools
   *
   * @param world world used by tools
   * @param engineControl engine controls used by tools
   * @return configured assistant
   */
  def build(world: World, engineControl: EngineControl): Langchain4jAssistant

/**
 * Default [[Langchain4jAssistantBuilder]] implementation.
 *
 * @param chatModel model used to generate responses
 */
case class Langchain4jAssistantFactory(chatModel: ChatModel) extends Langchain4jAssistantBuilder:

  override def build(world: World, engineControl: EngineControl): Langchain4jAssistant =
    AiServices
      .builder(classOf[Langchain4jAssistant])
      .chatModel(chatModel)
      .chatMemoryProvider(_ => MessageWindowChatMemory.withMaxMessages(10))
      .tools(Langchain4jTools()(using world, engineControl))
      .build()
