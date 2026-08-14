package monad_core.simulator.domain.ai.agent_evaluator

import monad_core.simulator.domain.ai.agent_evaluation.ToolCall
import org.scalatest.Inside.inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ToolCallTest extends AnyFunSuite with Matchers:

  private val entityId        = "entity-1"
  private val circleId        = "circle-1"
  private val rectangleId     = "rectangle-1"
  private val surfaceId       = "surface-1"
  private val teamId          = "team-1"
  private val x               = 10.0
  private val y               = 20.0
  private val defaultSize     = 5.0
  private val radius          = defaultSize
  private val height          = defaultSize
  private val rectangleLength = 8.0
  private val optionalTeamId  = Some(teamId)
  private val weight          = Some(2)
  private val speedX          = Some(1.5)
  private val speedY          = Some(2.5)

  test("can create entity query ToolCalls"):
    ToolCall.GetAllEntities shouldBe ToolCall.GetAllEntities

    inside(ToolCall.GetEntity(entityId)):
      case ToolCall.GetEntity(id) => id shouldBe entityId

  test("can create a CreateCircleEntity ToolCall"):
    val result = ToolCall.CreateCircleEntity(
      circleId,
      x,
      y,
      radius,
      optionalTeamId,
      weight,
      speedX,
      speedY
    )

    inside(result):
      case ToolCall.CreateCircleEntity(
            id,
            resultX,
            resultY,
            resultRadius,
            resultTeamId,
            resultWeight,
            resultSpeedX,
            resultSpeedY
          ) =>
        id shouldBe circleId
        resultX shouldBe x
        resultY shouldBe y
        resultRadius shouldBe radius
        resultTeamId shouldBe optionalTeamId
        resultWeight shouldBe weight
        resultSpeedX shouldBe speedX
        resultSpeedY shouldBe speedY

  test("can create a CreateRectangleEntity ToolCall"):
    val result = ToolCall.CreateRectangleEntity(
      rectangleId,
      x,
      y,
      height,
      rectangleLength,
      optionalTeamId,
      weight,
      speedX,
      speedY
    )

    inside(result):
      case ToolCall.CreateRectangleEntity(
            id,
            resultX,
            resultY,
            resultHeight,
            resultLength,
            resultTeamId,
            resultWeight,
            resultSpeedX,
            resultSpeedY
          ) =>
        id shouldBe rectangleId
        resultX shouldBe x
        resultY shouldBe y
        resultHeight shouldBe height
        resultLength shouldBe rectangleLength
        resultTeamId shouldBe optionalTeamId
        resultWeight shouldBe weight
        resultSpeedX shouldBe speedX
        resultSpeedY shouldBe speedY

  test("can create entity update ToolCalls"):
    inside(ToolCall.UpdateCircleEntity(circleId, x, y, radius)):
      case ToolCall.UpdateCircleEntity(id, resultX, resultY, resultRadius) =>
        (id, resultX, resultY, resultRadius) shouldBe (circleId, x, y, radius)

    inside(ToolCall.UpdateRectangleEntity(rectangleId, x, y, height, rectangleLength)):
      case ToolCall.UpdateRectangleEntity(id, resultX, resultY, resultHeight, resultLength) =>
        (id, resultX, resultY, resultHeight, resultLength) shouldBe (
          rectangleId,
          x,
          y,
          height,
          rectangleLength
        )

  test("can create a RemoveEntity ToolCall"):
    inside(ToolCall.RemoveEntity(entityId)):
      case ToolCall.RemoveEntity(id) => id shouldBe entityId

  test("can create surface query ToolCalls"):
    ToolCall.GetAllSurfaces shouldBe ToolCall.GetAllSurfaces

    inside(ToolCall.GetSurface(surfaceId)):
      case ToolCall.GetSurface(id) => id shouldBe surfaceId

  test("can create surface creation ToolCalls"):
    inside(ToolCall.CreateCircleSurface(circleId, x, y, radius)):
      case ToolCall.CreateCircleSurface(id, resultX, resultY, resultRadius) =>
        (id, resultX, resultY, resultRadius) shouldBe (circleId, x, y, radius)

    inside(ToolCall.CreateRectangleSurface(rectangleId, x, y, height, rectangleLength)):
      case ToolCall.CreateRectangleSurface(id, resultX, resultY, resultHeight, resultLength) =>
        (id, resultX, resultY, resultHeight, resultLength) shouldBe (
          rectangleId,
          x,
          y,
          height,
          rectangleLength
        )

  test("can create surface update ToolCalls"):
    inside(ToolCall.UpdateCircleSurface(circleId, x, y, radius)):
      case ToolCall.UpdateCircleSurface(id, resultX, resultY, resultRadius) =>
        (id, resultX, resultY, resultRadius) shouldBe (circleId, x, y, radius)

    inside(ToolCall.UpdateRectangleSurface(rectangleId, x, y, height, rectangleLength)):
      case ToolCall.UpdateRectangleSurface(id, resultX, resultY, resultHeight, resultLength) =>
        (id, resultX, resultY, resultHeight, resultLength) shouldBe (
          rectangleId,
          x,
          y,
          height,
          rectangleLength
        )

  test("can create a RemoveSurface ToolCall"):
    inside(ToolCall.RemoveSurface(surfaceId)):
      case ToolCall.RemoveSurface(id) => id shouldBe surfaceId

  test("can create team query ToolCalls"):
    ToolCall.GetAllTeams shouldBe ToolCall.GetAllTeams

    inside(ToolCall.GetTeam(teamId)):
      case ToolCall.GetTeam(id) => id shouldBe teamId

  test("can create team save ToolCalls"):
    val createEnemies = "team-2,team-3"
    val updateEnemies = "team-4"

    inside(ToolCall.CreateTeam(teamId, createEnemies)):
      case ToolCall.CreateTeam(id, enemies) =>
        id shouldBe teamId
        enemies shouldBe createEnemies

    inside(ToolCall.UpdateTeam(teamId, updateEnemies)):
      case ToolCall.UpdateTeam(id, enemies) =>
        id shouldBe teamId
        enemies shouldBe updateEnemies

  test("can create a RemoveTeam ToolCall"):
    inside(ToolCall.RemoveTeam(teamId)):
      case ToolCall.RemoveTeam(id) => id shouldBe teamId

  test("can create engine ToolCalls"):
    ToolCall.Start shouldBe ToolCall.Start
    ToolCall.Stop shouldBe ToolCall.Stop
