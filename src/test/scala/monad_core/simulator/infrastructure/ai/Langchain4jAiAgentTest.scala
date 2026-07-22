package monad_core.simulator.infrastructure.ai

import dev.langchain4j.memory.ChatMemory
import monad_core.simulator.application.ai.{AskAgentCommand, CleanHistoryCommand}
import monad_core.simulator.domain.ai.{AgentInfo, AgentResponse, ConversationId, UserPrompt}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues.*

class Langchain4jAiAgentTest extends AnyFunSuite with Matchers with Inside with MockFactory with ScalaFutures:

  test("ask returns the langchain4j assistant response"):
    val prompt = UserPrompt.from("Hi Jimmy how are you?").value
    val conversationId = ConversationId.from("chat1").value
    val response = "Well"
    val mockAssistant = mock[Langchain4jAssistant]
    val mockAgentInfo = mock[AgentInfo]
    val aiAgent = Langchain4jAiAgent(mockAssistant, mockAgentInfo)
    val askAgentCommand = AskAgentCommand(conversationId, prompt)

    mockAssistant.chat.expects(conversationId, prompt.toString).returns(response).once()

    val result = aiAgent.ask(askAgentCommand)

    inside(result.futureValue):
      case Right(value) => value shouldBe AgentResponse(response, 0)

  test("cleanHistory can clean assistant message history"):
    val conversationId = ConversationId.from("chat1").value
    val mockAssistant = mock[Langchain4jAssistant]
    val mockAgentInfo = mock[AgentInfo]
    val chatMemory = mock[ChatMemory]
    val aiAgent = Langchain4jAiAgent(mockAssistant, mockAgentInfo)
    val cleanHistoryCommand = CleanHistoryCommand(conversationId)

    mockAssistant.getChatMemory.expects(conversationId).returns(chatMemory).once()
    (() => chatMemory.clear()).expects().once()

    val result = aiAgent.cleanHistory(cleanHistoryCommand)

    result shouldBe Right(())
