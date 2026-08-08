package monad_core.simulator.infrastructure.ai

import monad_core.simulator.application.engine.world.World
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.domain.ai.AgentInfo
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Langchain4jAgentFactoryTest extends AnyFunSuite with Matchers with MockFactory:

  test("langchain4j factory can build an AiService"):
    val ollamaConfig = Langchain4jOllamaConfig(
      url = "http://127.0.0.1:11434",
      modelName = "gemma4:e2b",
      provider = "ollama"
    )
    val word: World = mock[World]
    val gameEngineRuntime: GameEngineRuntime = mock[GameEngineRuntime]

    val result = Langchain4jAgentFactory.buildOllama(ollamaConfig)(using word, gameEngineRuntime)

    result.agentInfo shouldBe AgentInfo(ollamaConfig.provider, ollamaConfig.modelName)
    result shouldBe a[Langchain4jAiAgent]