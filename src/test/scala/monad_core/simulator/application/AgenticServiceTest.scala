package monad_core.simulator.application

import dev.langchain4j.data.message.{AiMessage, UserMessage}
import dev.langchain4j.model.chat.ChatModel
import dev.langchain4j.model.chat.request.ChatRequest
import dev.langchain4j.model.chat.response.ChatResponse
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgenticServiceTest extends AnyFunSuite with Matchers:

  val ollamaProvider = "OLLAMA"

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

    response shouldBe Right(modelResponse)

  test("provides information about the configured model"):
    val testModel = "test-model"

    val config = AgentService.OllamaConfig(
      baseUrl = "http://localhost",
      modelName = testModel
    )

    val agentService = AgentService.ollama(config)

    agentService.modelInfo shouldBe ModelInfo(
      provider = ollamaProvider,
      model = testModel
    )

  test("provides information about the default model"):
    summon[AgentService].modelInfo shouldBe ModelInfo(
      provider = ollamaProvider,
      model = AgentService.OllamaConfig().modelName
    )
