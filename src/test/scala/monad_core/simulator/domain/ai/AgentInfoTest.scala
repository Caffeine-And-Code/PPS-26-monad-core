package monad_core.simulator.domain.ai

import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgentInfoTest extends AnyFunSuite with Matchers with Inside:

  test("can create an agent info"):
    val provider = "Ollama"
    val modelName = "llama:20b"

    val result = AgentInfo.from(provider, modelName)

    inside(result):
      case Right(p, m) =>
        p shouldBe provider
        m shouldBe modelName


