package monad_core.simulator.domain.ai

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgentResponseTest extends AnyFunSuite with Matchers :

  test("can create an agent response"):
    val response = "I am Jimmy your assistant"

    val result = AgentResponse(response)

    result.response shouldBe response

  test("can create an agent response error"):
    val error = "Impossible to load API"

    val result = AgentResponseError(error)

    result.message shouldBe error

