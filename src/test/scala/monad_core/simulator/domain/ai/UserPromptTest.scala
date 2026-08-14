package monad_core.simulator.domain.ai

import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class UserPromptTest extends AnyFunSuite with Matchers with Inside:

  test("can create a user prompt from a valid prompt"):
    val prompt = "Hy Jimmy how are you?"

    val result = UserPrompt.from(prompt)

    inside(result):
      case Right(validPrompt) => validPrompt.toString shouldBe prompt

  test("cannot create a user prompt from an invalid prompt"):
    val invalidPrompt = "    "

    val result = UserPrompt.from(invalidPrompt)

    inside(result):
      case Left(error) => error shouldBe InvalidUserPrompt()
