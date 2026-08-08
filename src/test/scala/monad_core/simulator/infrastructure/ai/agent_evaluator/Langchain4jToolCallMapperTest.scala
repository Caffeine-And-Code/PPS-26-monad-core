package monad_core.simulator.infrastructure.ai.agent_evaluator

import dev.langchain4j.agent.tool.ToolExecutionRequest
import monad_core.simulator.domain.ai.agent_evaluation.ToolCall
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.*

class Langchain4jToolCallMapperTest extends AnyFunSuite with Matchers:

  private val entityId = "entity-1"
  private val circleId = "circle-1"
  private val rectangleId = "rectangle-1"
  private val surfaceId = "surface-1"
  private val teamId = "team-1"
  private val enemies = "team-2,team-3"
  private val x = 10.0
  private val y = 20.0
  private val radius = 5.0
  private val height = 6.0
  private val rectangleLength = 8.0
  private val weight = 2
  private val speedX = 1.5
  private val speedY = 2.5
  private val mapper = Langchain4jToolCallMapper()

  test("can map Langchain4j tool requests"):
    val cases = Table(
      ("name", "arguments", "expected"),
      ("getAllEntities", "{}", ToolCall.GetAllEntities),
      ("getEntity", s"""{"id":"$entityId"}""", ToolCall.GetEntity(entityId)),
      (
        "createCircleEntity",
        entityArguments(circleId, s""""radius":$radius"""),
        ToolCall.CreateCircleEntity(
          circleId,
          x,
          y,
          radius,
          Some(teamId),
          Some(weight),
          Some(speedX),
          Some(speedY)
        )
      ),
      (
        "createRectangleEntity",
        entityArguments(rectangleId, s""""height":$height,"length":$rectangleLength"""),
        ToolCall.CreateRectangleEntity(
          rectangleId,
          x,
          y,
          height,
          rectangleLength,
          Some(teamId),
          Some(weight),
          Some(speedX),
          Some(speedY)
        )
      ),
      ("updateCircleEntity", circleArguments(circleId), ToolCall.UpdateCircleEntity(circleId, x, y, radius)),
      (
        "updateRectangleEntity",
        rectangleArguments(rectangleId),
        ToolCall.UpdateRectangleEntity(rectangleId, x, y, height, rectangleLength)
      ),
      ("removeEntity", s"""{"id":"$entityId"}""", ToolCall.RemoveEntity(entityId)),
      ("getAllSurfaces", "{}", ToolCall.GetAllSurfaces),
      ("getSurface", s"""{"id":"$surfaceId"}""", ToolCall.GetSurface(surfaceId)),
      ("createCircleSurface", circleArguments(circleId), ToolCall.CreateCircleSurface(circleId, x, y, radius)),
      (
        "createRectangleSurface",
        rectangleArguments(rectangleId),
        ToolCall.CreateRectangleSurface(rectangleId, x, y, height, rectangleLength)
      ),
      ("updateCircleSurface", circleArguments(circleId), ToolCall.UpdateCircleSurface(circleId, x, y, radius)),
      (
        "updateRectangleSurface",
        rectangleArguments(rectangleId),
        ToolCall.UpdateRectangleSurface(rectangleId, x, y, height, rectangleLength)
      ),
      ("removeSurface", s"""{"id":"$surfaceId"}""", ToolCall.RemoveSurface(surfaceId)),
      ("getAllTeams", "{}", ToolCall.GetAllTeams),
      ("getTeam", s"""{"id":"$teamId"}""", ToolCall.GetTeam(teamId)),
      ("createTeam", teamArguments, ToolCall.CreateTeam(teamId, enemies)),
      ("updateTeam", teamArguments, ToolCall.UpdateTeam(teamId, enemies)),
      ("removeTeam", s"""{"id":"$teamId"}""", ToolCall.RemoveTeam(teamId)),
      ("start", "{}", ToolCall.Start),
      ("stop", "{}", ToolCall.Stop)
    )

    forAll(cases):
      (name, arguments, expected) =>
        mapper.from(request(name, arguments)) shouldBe Right(expected)

  test("cannot map an unknown tool request"):
    val toolName = "unknownTool"

    val result = mapper.from(request(toolName, "{}"))

    result shouldBe Left(InvalidToolCall(toolName, "unknown tool"))

  test("can map omitted optional entity arguments"):
    val arguments = s"""{"id":"$circleId","x":$x,"y":$y,"radius":$radius}"""
    val expected = ToolCall.CreateCircleEntity(circleId, x, y, radius)

    val result = mapper.from(request("createCircleEntity", arguments))

    result shouldBe Right(expected)

  test("cannot map a tool request with missing arguments"):
    val result = mapper.from(request("getEntity", "{}"))

    result shouldBe a[Left[InvalidToolCall, ?]]

  private def request(name: String, arguments: String): ToolExecutionRequest =
    ToolExecutionRequest.builder()
      .name(name)
      .arguments(arguments)
      .build()

  private def entityArguments(id: String, shapeArguments: String): String =
    s"""{"id":"$id","x":$x,"y":$y,$shapeArguments,"teamId":"$teamId","weight":$weight,"speedX":$speedX,"speedY":$speedY}"""

  private def circleArguments(id: String): String =
    s"""{"id":"$id","x":$x,"y":$y,"radius":$radius}"""

  private def rectangleArguments(id: String): String =
    s"""{"id":"$id","x":$x,"y":$y,"height":$height,"length":$rectangleLength}"""

  private def teamArguments: String =
    s"""{"id":"$teamId","enemies":"$enemies"}"""
