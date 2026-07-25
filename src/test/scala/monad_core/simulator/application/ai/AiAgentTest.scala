package monad_core.simulator.application.ai

import monad_core.simulator.domain.ai.{ConversationId, UserPrompt}
import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AiAgentTest extends AnyFunSuite with Matchers :

  test("can create an ask agent command"):
    val conversationId = ConversationId.from("chat1").value
    val prompt = UserPrompt.from("Hi Jimmy").value

    val result = AskAgentCommand(conversationId, prompt)

    result.conversationId shouldBe conversationId
    result.prompt shouldBe prompt

  test("can create a clean history command"):
    val conversationId = ConversationId.from("chat1").value

    val result = CleanHistoryCommand(conversationId)

    result.conversationId shouldBe conversationId
