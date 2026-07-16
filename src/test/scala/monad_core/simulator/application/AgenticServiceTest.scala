package monad_core.simulator.application

import dev.langchain4j.data.message.{AiMessage, UserMessage}
import dev.langchain4j.model.chat.ChatModel
import dev.langchain4j.model.chat.request.ChatRequest
import dev.langchain4j.model.chat.response.ChatResponse
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgenticServiceTest extends AnyFunSuite with Matchers:

  private def fakeModel(prompt: String, response: String): ChatModel =
    new ChatModel:
      override def doChat(request: ChatRequest): ChatResponse =
        request.messages().getFirst.asInstanceOf[UserMessage].singleText() shouldBe prompt
        ChatResponse.builder().aiMessage(AiMessage.from(response)).build()

  test("can send a message to the AI agent"):
    val simplePrompt = "How are you today"
    val modelResponse = "I'm fine!"
    val model = fakeModel(simplePrompt, modelResponse)

    val agentService = AgentService(model)

    val response = agentService.ask(simplePrompt)

    response shouldBe modelResponse

  test("provides information about the configured model"):
    val config = AgentService.OllamaConfig(
      baseUrl = "http://localhost",
      modelName = "test-model"
    )

    val agentService = AgentService.ollama(config)

    agentService.modelInfo shouldBe ModelInfo(
      provider = "OLLAMA",
      model = "test-model"
    )