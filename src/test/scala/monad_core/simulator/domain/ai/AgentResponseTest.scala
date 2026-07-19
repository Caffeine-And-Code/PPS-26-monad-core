package monad_core.simulator.domain.ai

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgentResponseTest extends AnyFunSuite with Matchers {

  test("can create an agent response"):
    val response = "I am Jimmy your assistant"
    val tokenUsed = 27

    val result = AgentResponse(response, tokenUsed)

    result.response shouldBe response
    result.tokenUsed shouldBe tokenUsed
}
