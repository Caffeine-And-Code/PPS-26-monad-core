package monad_core.simulator.infrastructure.ai

import dev.langchain4j.service.{MemoryId, SystemMessage, UserMessage}
import dev.langchain4j.service.memory.ChatMemoryAccess
import monad_core.simulator.domain.ai.ConversationId

trait Langchain4jAssistant extends ChatMemoryAccess:

  @SystemMessage(Array("You are an helpful assistant, your name is Jimmy and you can talk only about Geometry, nothing else"))
  def chat(@MemoryId memoryId: ConversationId, @UserMessage message: String): String