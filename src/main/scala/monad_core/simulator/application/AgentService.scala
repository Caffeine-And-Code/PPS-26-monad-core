package monad_core.simulator.application

import dev.langchain4j.model.chat.ChatModel

trait AgentService:
  def ask(message: String): String

object AgentService:

  given ollamaAgentService(using model: ChatModel): AgentService with
    override def ask(message: String): String =
      model.chat(message)
