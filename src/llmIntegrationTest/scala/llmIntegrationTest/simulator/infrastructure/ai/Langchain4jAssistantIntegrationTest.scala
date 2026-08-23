package llmIntegrationTest.simulator.infrastructure.ai

import llmIntegrationTest.langchain4j.judge.LlmJudgeAssistant
import llmIntegrationTest.langchain4j.matchers.AssistantResponseMatchers.*
import llmIntegrationTest.langchain4j.matchers.LLMAsAJudgeMatchers.*
import llmIntegrationTest.langchain4j.matchers.ToolExecutionMatchers.*
import monad_core.engine.model.{Entity, LocatableId, Vector2D}
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.{SaveEntityCommand, World}
import monad_core.simulator.domain.ai.ConversationId
import monad_core.simulator.infrastructure.ai.{
  Langchain4jAgentFactory,
  Langchain4jAssistant,
  Langchain4jOllamaConfig
}
import org.scalactic.Prettifier.default
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.compiletime.uninitialized

class Langchain4jAssistantIntegrationTest extends AnyFunSuite with Matchers with MockFactory with BeforeAndAfterEach:

  val circleEntity: Entity = Entity.circle("e1", Vector2D(20, 20), 50)
    .value
    .withWeight(70)
    .value
    .withSpeed(Vector2D(20, 35))

  val rectangleEntity: Entity = Entity.rectangle("e2", Vector2D(4, 3), 11, 9)
    .value
    .withWeight(20)
    .value

  val ollamaConfig = Langchain4jOllamaConfig(
    url = "http://localhost:11434",
    modelName = "gemma4:e4b"
  )

  val memoryId:ConversationId = ConversationId.from("chat1").value

  private var world: World = uninitialized
  private var gameEngineRuntime: GameEngineRuntime = uninitialized

  override def beforeEach(): Unit =
    super.beforeEach()
    world = mock[World]
    gameEngineRuntime = mock[GameEngineRuntime]

  private def getJudgeModel: LlmJudgeAssistant =
    LlmJudgeAssistant.buildOllama(ollamaConfig)

  private def getAssistant: Langchain4jAssistant =
    Langchain4jAgentFactory.buildOllama(ollamaConfig)(
      using world,
      gameEngineRuntime
    ).assistant

  test("Can talk to assistant about something that not requires tools and it not call tools"):
    val userMessage = "Hi, what's your name, what can you do?"
    val assistant = getAssistant

    val result = assistant.chat(memoryId, userMessage)

    result should containsInResponse("jimmy")
    result should notExecuteTools

  test("Can get all entities in the scene and returns them"):
    val userMessage = "witch entities are in the scene?"
    val assistant = getAssistant

    (() => world.getAllEntities).expects().returns(List(circleEntity, rectangleEntity)).once()

    val result = assistant.chat(memoryId, userMessage)

    result should containsInResponse(circleEntity.id.toString)
    result should containsInResponse(rectangleEntity.id.toString)
    result should onlyExecuteTool("getAllEntities")

  test("Can create a circular entity using the assistant when game engine is in pause"):
    val userMessage = "Create a circle entity with id e1 with center in 20,20 and a radius of 50, his weight is 70, and a speed of 20,35"
    val assistant = getAssistant

    (() => gameEngineRuntime.isRunning).expects().returning(false).once()
    world.createEntity.expects(SaveEntityCommand(entity = circleEntity)).returning(Right(())).once()

    val result = assistant.chat(memoryId, userMessage)

    result should onlyExecuteTool("createCircleEntity")

  test("Can execute different tools in succession"):
    val userMessage = "First list all entities in the scene, then start the game engine."
    val assistant = getAssistant

    (() => world.getAllEntities).expects().returning(List(circleEntity, rectangleEntity)).once()
    (() => gameEngineRuntime.start()).expects().once()

    val result = assistant.chat(memoryId, userMessage)

    result should containsInResponse(List(circleEntity.id.toString, rectangleEntity.id.toString))
    result should executeOnlyTheseTools(List("getAllEntities", "start"))

  test("Can update an existing entity"):
    val firstMessage = "Get the circle entity with id 'my_circle' from the scene"
    val secondMessage = "update his radius to 10"
    val circle = Entity.circle("my_circle", Vector2D(20, 20), 67).value
    val updatedEntity = Entity.circle("my_circle", Vector2D(20, 20), 10).value
    val assistant = getAssistant

    (() => gameEngineRuntime.isRunning).expects().returning(false).once()
    world.getEntity.expects(LocatableId("my_circle").value.value).returning(Right(circle)).once()
    world.updateEntity.expects(SaveEntityCommand(updatedEntity)).returning(Right(())).once()

    val firstResponse = assistant.chat(memoryId, firstMessage)
    val secondResponse = assistant.chat(memoryId, secondMessage)

    firstResponse should onlyExecuteTool("getEntity")
    secondResponse should onlyExecuteTool("updateCircleEntity")

  test("Can remember information across messages in the same conversation"):
    val firstMessage = "Remember that my preferred shape is the circle."
    val secondMessage = "What shape did I tell you I prefer?"
    val assistant = getAssistant
    val judge = getJudgeModel

    val firstResult = assistant.chat(memoryId, firstMessage)
    val secondResult = assistant.chat(memoryId, secondMessage)

    firstResult should notExecuteTools
    secondResult judgedBy judge shouldSatisfy
      "should tell that circle is the preferred shape"
    secondResult should notExecuteTools

  test("Asks for clarification when required information is missing"):
    val userMessage = "Create a circle named e1 at 20,20."
    val assistant = getAssistant
    val judgeAssistant = getJudgeModel

    val result = assistant.chat(memoryId, userMessage)

    result should notExecuteTools
    result judgedBy judgeAssistant shouldSatisfy
      "The assistant should ask for the missing radius without inventing it."

  test("Cannot create entities while engine is running"):
    val userMessage = "Create a circle entity with id e1 with center in 20,20 and a radius of 50, his weight is 70, and a speed of 20,35"
    val assistant = getAssistant
    val judgeAssistant = getJudgeModel

    (() => gameEngineRuntime.isRunning).expects().returning(true).once()
    world.createEntity.expects(*).never()

    val result = assistant.chat(memoryId, userMessage)

    val judgeCriteria =
      "The assistant should say that the action can not be done because the engine is running."
    result judgedBy judgeAssistant shouldSatisfy judgeCriteria
    result should onlyExecuteTool("createCircleEntity")

  test("Assistant can talk only about The application tools and geometry, not on any other things"):
    val userMessage = "Witch is the capital of Italy?"
    val assistant = getAssistant
    val judgeAssistant = getJudgeModel

    val result = assistant.chat(memoryId, userMessage)

    val judgeCriteria = "Assistant should refuse to answer user question cause it can talk about his allowed topics"
    result judgedBy judgeAssistant shouldSatisfy judgeCriteria
    result should notExecuteTools

  test("Assistant should response with same languages as user question"):
    val userMessage = "Come ti chiami, chi sei?"
    val assistant = getAssistant
    val judgeAssistant = getJudgeModel

    val result = assistant.chat(memoryId, userMessage)

    val judgeCriteria = "Assistant should response in same language as user question, Italian in this case"
    result judgedBy judgeAssistant shouldSatisfy judgeCriteria
    result should notExecuteTools
