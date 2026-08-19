package monad_core.simulator.domain.ai

import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgentInfoTest extends AnyFunSuite with Matchers with Inside:

  private val provider  = "Ollama"
  private val modelName = "llama:20b"

  test("can create an agent info"):
    val result = AgentInfo.from(provider, modelName)

    inside(result):
      case Right(p, m) =>
        p shouldBe provider
        m shouldBe modelName

  test("cannot create an agent info with invalid provider"):
    val invalidProvider = "   "

    val result = AgentInfo.from(invalidProvider, modelName)

    result shouldBe Left(InvalidProviderName())

  test("cannot create an agent info with invalid modelName"):
    val invalidModelName = "   "

    val result = AgentInfo.from(provider, invalidModelName)

    result shouldBe Left(InvalidModelName())
