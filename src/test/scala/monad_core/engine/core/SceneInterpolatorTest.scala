package monad_core.engine.core

import monad_core.engine.helper.DummyEntityHelper.{makeFixedEntityCircle, makeFixedEntityRectangle}
import monad_core.engine.helper.DummySurfaceHelper.makeSurfaceCircle
import monad_core.engine.helper.DummyTeamHelper.makeTeam
import monad_core.engine.helper.MockSceneHelper
import monad_core.engine.model.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SceneInterpolatorTest extends AnyFunSuite with Matchers with MockFactory with MockSceneHelper:

  private val PreviousBounds = WorldBounds(100.0, 100.0).value
  private val NextBounds     = WorldBounds(200.0, 300.0).value

  test("SceneInterpolator should reject an alpha lower than zero"):
    val previous = sceneWithEntities(List.empty)
    val next     = sceneWithEntities(List.empty)

    val result = SceneInterpolator(previous, next, -0.1)

    result shouldBe Left(InvalidInterpolationAlpha(-0.1))

  test("SceneInterpolator should reject an alpha greater than one"):
    val previous = sceneWithEntities(List.empty)
    val next     = sceneWithEntities(List.empty)

    val result = SceneInterpolator(previous, next, 1.1)

    result shouldBe Left(InvalidInterpolationAlpha(1.1))

  test("SceneInterpolator should interpolate an entity position"):
    val previousEntity = makeFixedEntityCircle(position = Vector2D(0.0, 10.0))
    val nextEntity     = previousEntity.moveTo(Vector2D(100.0, 50.0))
    val alpha          = 0.25

    val result = SceneInterpolator(
      sceneWithEntities(List(previousEntity)),
      sceneWithEntities(List(nextEntity)),
      alpha
    ).value

    result.entities(previousEntity.id).position shouldBe Vector2D(25.0, 20.0)

  test("SceneInterpolator should use surfaces from the next scene without interpolating them"):
    val previousSurface = makeSurfaceCircle(position = Vector2D(10.0, 20.0))
    val nextSurface     = makeSurfaceCircle(position = Vector2D(30.0, 60.0))

    val result = SceneInterpolator(
      sceneWithSurfaces(List.empty, List(previousSurface)),
      sceneWithSurfaces(List.empty, List(nextSurface)),
      0.5
    ).value

    result.surfaces(previousSurface.id) shouldBe nextSurface

  test("SceneInterpolator should use the next scene metadata"):
    val previousEntity = makeFixedEntityCircle()
    val nextEntity = previousEntity
      .moveTo(Vector2D(10.0, 10.0))
      .withSpeed(Vector2D(4.0, 5.0))

    val result = SceneInterpolator(
      sceneWithEntities(List(previousEntity)),
      sceneWithEntities(List(nextEntity)),
      0.5
    ).value

    result.entities(previousEntity.id).speed shouldBe nextEntity.speed
    result.entities(previousEntity.id).shape shouldBe nextEntity.shape

  test("SceneInterpolator should interpolate world bounds"):
    val result = SceneInterpolator(
      sceneWithBounds(PreviousBounds),
      sceneWithBounds(NextBounds),
      0.5
    ).value

    result.bounds.lowerRight shouldBe Vector2D(150.0, 200.0)

  test("SceneInterpolator should use the next scene topology"):
    val previousEntity = makeFixedEntityCircle(id = "removed")
    val nextEntity = makeFixedEntityCircle(
      id = "added",
      position = Vector2D(10.0, 10.0)
    )

    val result = SceneInterpolator(
      sceneWithEntities(List(previousEntity)),
      sceneWithEntities(List(nextEntity)),
      0.5
    ).value

    result.entities.keySet shouldBe Set(nextEntity.id)

  test("SceneInterpolator should return the next scene at alpha one"):
    val previousEntity = makeFixedEntityCircle()
    val nextEntity     = previousEntity.moveTo(Vector2D(10.0, 15.0))
    val nextTeam       = makeTeam("team")

    val result = SceneInterpolator(
      sceneWithEntities(List(previousEntity)),
      sceneWithTeams(List(nextEntity), List(nextTeam)),
      1.0
    ).value

    result.entities shouldBe Map(nextEntity.id -> nextEntity)
    result.teams shouldBe Map(nextTeam.id -> nextTeam)

  test("SceneInterpolator should interpolate rotation across the shortest arc"):
    val previousEntity = makeFixedEntityRectangle(
      id = "rotating",
      position = Vector2D(10.0, 10.0),
      width = 2.0,
      height = 4.0
    ).rotateTo(350.0).value
    val nextEntity = previousEntity.rotateTo(10.0).value

    val result = SceneInterpolator(
      sceneWithEntities(List(previousEntity)),
      sceneWithEntities(List(nextEntity)),
      0.5
    ).value

    result.entities(previousEntity.id).rotation shouldBe 0.0

  test("SceneInterpolator should interpolate position and rotation together"):
    val previousEntity = makeFixedEntityRectangle(
      id = "moving-and-rotating",
      position = Vector2D(10.0, 20.0)
    ).rotateTo(30.0).value
    val nextEntity = previousEntity
      .moveTo(Vector2D(30.0, 60.0))
      .rotateTo(90.0)
      .value

    val result = SceneInterpolator(
      sceneWithEntities(List(previousEntity)),
      sceneWithEntities(List(nextEntity)),
      0.5
    ).value

    val interpolatedEntity = result.entities(previousEntity.id)
    interpolatedEntity.position shouldBe Vector2D(20.0, 40.0)
    interpolatedEntity.rotation shouldBe 60.0
