package monad_core.simulator.application

import dev.langchain4j.data.message.{AiMessage, UserMessage}
import dev.langchain4j.model.chat.ChatModel
import dev.langchain4j.model.chat.request.ChatRequest
import dev.langchain4j.model.chat.response.ChatResponse
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgenticServiceTest extends AnyFunSuite with Matchers:

  private def getMockedModel(prompt:String, response:String):ChatModel =
    new ChatModel:
      override def doChat(request: ChatRequest): ChatResponse =
        request.messages().getFirst.asInstanceOf[UserMessage].singleText() shouldBe prompt
        ChatResponse.builder().aiMessage(AiMessage.from(response)).build()

  test("can send a message to the AI agent"):
    val simplePrompt = "How are you today"
    val modelResponse = "I'm fine!"
    val model = getMockedModel(simplePrompt, modelResponse)

    val agenticService = AgentService.ollamaAgentService(using model)

    val response = agenticService.ask(simplePrompt)

    response shouldBe modelResponse
