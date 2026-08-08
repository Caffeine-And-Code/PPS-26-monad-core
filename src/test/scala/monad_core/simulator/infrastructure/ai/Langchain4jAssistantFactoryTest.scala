package monad_core.simulator.infrastructure.ai

import dev.langchain4j.model.chat.ChatModel
import monad_core.simulator.application.engine.EngineControl
import monad_core.simulator.application.engine.world.World
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Langchain4jAssistantFactoryTest extends AnyFunSuite with Matchers with MockFactory:

  test("can build a Langchain4jAssistant"):
    val chatModel = mock[ChatModel]
    val world = mock[World]
    val engineControl = mock[EngineControl]
    val factory = Langchain4jAssistantFactory(chatModel)

    val result = factory.build(world, engineControl)

    result shouldBe a[Langchain4jAssistant]
