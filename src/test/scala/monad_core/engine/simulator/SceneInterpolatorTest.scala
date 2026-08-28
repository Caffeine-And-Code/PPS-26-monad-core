package monad_core.engine.simulator

import monad_core.engine.core.InvalidInterpolationAlpha
import monad_core.engine.helper.DummyEntityHelper.{
  makeFixedEntityCircle,
  makeFixedEntityRectangle,
  makeMovingEntityRectangle
}
import monad_core.engine.helper.DummySurfaceHelper.makeSurfaceCircle
import monad_core.engine.helper.MockStateHelper
import monad_core.engine.model.*
import monad_core.engine.physics.utils.Rotation
import monad_core.engine.simulator.StateInterpolator
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SceneInterpolatorTest extends AnyFunSuite with Matchers with MockFactory with MockStateHelper:

  private val PreviousBounds = WorldBounds(100.0, 100.0).value
  private val NextBounds     = WorldBounds(200.0, 300.0).value
  private val HalfAlpha      = 0.5
  private val ZeroAlpha      = 0.0
  private val StaticAlpha    = 1.0

  private case class InterpolatedValues(
      position: Vector2D,
      rotation: Double
  )

  private def expectedInterpolation(prev: Entity, next: Entity, alpha: Double): InterpolatedValues =
    val interpolatedPosition = prev.position + (next.position - prev.position) * alpha
    val interpolatedRotation = Rotation.interpolate(prev.rotation, next.rotation, alpha)
    InterpolatedValues(interpolatedPosition, interpolatedRotation)

  test("SceneInterpolator should reject an alpha lower than zero"):
    val previous = stateWithEntities(List.empty)
    val next     = stateWithEntities(List.empty)

    val result = StateInterpolator(previous, next, -0.1)

    result shouldBe Left(InvalidInterpolationAlpha(-0.1))

  test("SceneInterpolator should reject an alpha greater than one"):
    val previous = stateWithEntities(List.empty)
    val next     = stateWithEntities(List.empty)

    val result = StateInterpolator(previous, next, 1.1)

    result shouldBe Left(InvalidInterpolationAlpha(1.1))

  test("SceneInterpolator should interpolate an entity position"):
    val previousEntity = makeFixedEntityCircle(position = Vector2D(0.0, 10.0))
    val nextEntity     = previousEntity.moveTo(Vector2D(100.0, 50.0))
    val alpha          = 0.25

    val expected = expectedInterpolation(previousEntity, nextEntity, alpha).position

    val result = StateInterpolator(
      stateWithEntities(List(previousEntity)),
      stateWithEntities(List(nextEntity)),
      alpha
    ).value

    result.entities(previousEntity.id).position shouldBe expected

  test("SceneInterpolator should use surfaces from the next scene without interpolating them"):
    val previousSurface = makeSurfaceCircle(position = Vector2D(10.0, 20.0))
    val nextSurface     = makeSurfaceCircle(position = Vector2D(30.0, 60.0))

    val result = StateInterpolator(
      stateWithSurfaces(List.empty, List(previousSurface)),
      stateWithSurfaces(List.empty, List(nextSurface)),
      HalfAlpha
    ).value

    result.surfaces(previousSurface.id) shouldBe nextSurface

  test("SceneInterpolator should use the next scene metadata"):
    val previousEntity = makeFixedEntityCircle()
    val nextEntity = previousEntity
      .moveTo(Vector2D(10.0, 10.0))
      .withSpeed(Vector2D(4.0, 5.0))

    val result = StateInterpolator(
      stateWithEntities(List(previousEntity)),
      stateWithEntities(List(nextEntity)),
      HalfAlpha
    ).value

    result.entities(previousEntity.id).speed shouldBe nextEntity.speed
    result.entities(previousEntity.id).shape shouldBe nextEntity.shape

  test("SceneInterpolator should interpolate world bounds"):
    val result = StateInterpolator(
      stateWithBounds(PreviousBounds),
      stateWithBounds(NextBounds),
      HalfAlpha
    ).value

    result.bounds.lowerRight shouldBe Vector2D(150.0, 200.0)

  test("SceneInterpolator should use the next scene topology"):
    val previousEntity = makeFixedEntityCircle(id = "removed")
    val nextEntity = makeFixedEntityCircle(
      id = "added",
      position = Vector2D(10.0, 10.0)
    )

    val result = StateInterpolator(
      stateWithEntities(List(previousEntity)),
      stateWithEntities(List(nextEntity)),
      HalfAlpha
    ).value

    result.entities.keySet shouldBe Set(nextEntity.id)

  test("SceneInterpolator should return the next scene at alpha one"):
    val previousEntity = makeFixedEntityCircle(rotation = 30.0)
    val nextEntity     = previousEntity.moveTo(Vector2D(10.0, 15.0)).rotateTo(90.0).value

    val result = StateInterpolator(
      stateWithEntities(List(previousEntity)),
      stateWithEntities(List(nextEntity)),
      StaticAlpha
    ).value

    val resultEntity = result.entities(nextEntity.id)

    resultEntity.position shouldBe nextEntity.position
    resultEntity.rotation shouldBe nextEntity.rotation

  test("SceneInterpolator should return the previous scene at alpha zero"):
    val previousEntity = makeFixedEntityCircle(rotation = 30.0)
    val nextEntity     = previousEntity.moveTo(Vector2D(10.0, 15.0)).rotateTo(90.0).value

    val result = StateInterpolator(
      stateWithEntities(List(previousEntity)),
      stateWithEntities(List(nextEntity)),
      ZeroAlpha
    ).value

    val resultEntity = result.entities(nextEntity.id)

    resultEntity.position shouldBe previousEntity.position
    resultEntity.rotation shouldBe previousEntity.rotation

  test("SceneInterpolator should interpolate rotation across the shortest arc"):
    val previousEntity = makeFixedEntityRectangle(
      id = "rotating",
      position = Vector2D(10.0, 10.0),
      width = 2.0,
      height = 4.0
    ).rotateTo(350.0).value

    val nextEntity = previousEntity.rotateTo(10.0).value

    val expected = expectedInterpolation(previousEntity, nextEntity, HalfAlpha).rotation

    val result = StateInterpolator(
      stateWithEntities(List(previousEntity)),
      stateWithEntities(List(nextEntity)),
      HalfAlpha
    ).value

    result.entities(previousEntity.id).rotation shouldBe expected

  test("SceneInterpolator should interpolate position and rotation together"):
    val previousEntity = makeFixedEntityRectangle(
      id = "moving-and-rotating",
      position = Vector2D(10.0, 20.0)
    ).rotateTo(30.0).value

    val nextEntity = previousEntity
      .moveTo(Vector2D(30.0, 60.0))
      .rotateTo(90.0)
      .value

    val expected = expectedInterpolation(previousEntity, nextEntity, HalfAlpha)

    val result = StateInterpolator(
      stateWithEntities(List(previousEntity)),
      stateWithEntities(List(nextEntity)),
      HalfAlpha
    ).value

    val interpolatedEntity = result.entities(previousEntity.id)

    interpolatedEntity.position shouldBe expected.position
    interpolatedEntity.rotation shouldBe expected.rotation
