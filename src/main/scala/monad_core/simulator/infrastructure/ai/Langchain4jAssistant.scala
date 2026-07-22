package monad_core.simulator.infrastructure.ai

import dev.langchain4j.service.SystemMessage

trait Langchain4jAssistant:

  @SystemMessage(Array("You are an helpful assistant, your name is Jimmy and you can talk only about Geometry, nothing else"))
  def chat(message: String): String