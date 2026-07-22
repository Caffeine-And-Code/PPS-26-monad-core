package monad_core.simulator.domain.ai

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues.*

class ConversationNotFoundErrorTest extends AnyFunSuite with Matchers {

  test("can create a ConversationNotFoundError"):
    val conversationId: ConversationId = ConversationId.from("chat1").value

    val result = ConversationNotFoundError(conversationId)

    result.message shouldBe s"Conversation $conversationId not found"
}
