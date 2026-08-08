package monad_core.simulator.infrastructure.ai

import dev.langchain4j.memory.chat.MessageWindowChatMemory
import dev.langchain4j.model.chat.ChatModel
import dev.langchain4j.service.AiServices
import monad_core.simulator.application.engine.EngineControl
import monad_core.simulator.application.engine.world.World

trait Langchain4jAssistantBuilder:
  def build(world: World, engineControl: EngineControl): Langchain4jAssistant

case class Langchain4jAssistantFactory(chatModel: ChatModel) extends Langchain4jAssistantBuilder:

  override def build(world: World, engineControl: EngineControl): Langchain4jAssistant =
    AiServices.builder(classOf[Langchain4jAssistant])
      .chatModel(chatModel)
      .chatMemoryProvider(_ => MessageWindowChatMemory.withMaxMessages(10))
      .tools(Langchain4jTools()(using world, engineControl))
      .build()
