package monad_core.simulator.domain.ai

import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ConversationIdTest extends AnyFunSuite with Matchers with Inside:

  test("Can create a conversation id from a string"):
    val id = "chat1"

    val result = ConversationId.from(id)

    inside(result):
      case Right(resId) => resId.toString shouldBe id

  test("cannot create a conversation id from an invalid string"):
    val invalidId = "     "

    val result = ConversationId.from(invalidId)

    inside(result):
      case Left(error) => error shouldBe InvalidConversationId()

