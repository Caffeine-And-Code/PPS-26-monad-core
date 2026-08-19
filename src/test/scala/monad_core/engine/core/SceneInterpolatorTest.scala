package monad_core.engine.core

import monad_core.engine.core.traits.State
import monad_core.engine.model.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SceneInterpolatorTest extends AnyFunSuite with Matchers with MockFactory:

  private val PreviousBounds = WorldBounds(100.0, 100.0).value
  private val NextBounds     = WorldBounds(200.0, 300.0).value

  private def stateWith(
      entities: List[Entity] = List.empty,
      surfaces: List[Surface] = List.empty,
      teams: List[Team] = List.empty,
      bounds: WorldBounds = PreviousBounds
  ): State =
    val state = mock[State]

    (() => state.allEntities).expects().returning(entities).anyNumberOfTimes()
    (() => state.allSurfaces).expects().returning(surfaces).anyNumberOfTimes()
    (() => state.allTeams).expects().returning(teams).anyNumberOfTimes()
    (() => state.bounds).expects().returning(bounds).anyNumberOfTimes()

    state

  test("SceneInterpolator should reject an alpha lower than zero"):
    val previous = stateWith()
    val next     = stateWith()

    val result = SceneInterpolator(previous, next, -0.1)

    result shouldBe Left(InvalidInterpolationAlpha(-0.1))

  test("SceneInterpolator should reject an alpha greater than one"):
    val previous = stateWith()
    val next     = stateWith()

    val result = SceneInterpolator(previous, next, 1.1)

    result shouldBe Left(InvalidInterpolationAlpha(1.1))

  test("SceneInterpolator should interpolate an entity position"):
    val previousEntity = Entity.circle("entity", Vector2D(0.0, 10.0), 1.0).value
    val nextEntity     = previousEntity.moveTo(Vector2D(100.0, 50.0))
    val alpha          = 0.25

    val result = SceneInterpolator(
      stateWith(entities = List(previousEntity)),
      stateWith(entities = List(nextEntity)),
      alpha
    ).value

    result.entities(previousEntity.id).position shouldBe Vector2D(25.0, 20.0)

  test("SceneInterpolator should use surfaces from the next scene without interpolating them"):
    val previousSurface = Surface.circle("surface", Vector2D(10.0, 20.0), 2.0).value
    val nextSurface     = Surface.circle("surface", Vector2D(30.0, 60.0), 2.0).value

    val result = SceneInterpolator(
      stateWith(surfaces = List(previousSurface)),
      stateWith(surfaces = List(nextSurface)),
      0.5
    ).value

    result.surfaces(previousSurface.id) shouldBe nextSurface

  test("SceneInterpolator should use the next scene metadata"):
    val previousEntity = Entity.circle("entity", Vector2D(0.0, 0.0), 1.0).value
    val nextEntity = previousEntity
      .moveTo(Vector2D(10.0, 10.0))
      .withSpeed(Vector2D(4.0, 5.0))

    val result = SceneInterpolator(
      stateWith(entities = List(previousEntity)),
      stateWith(entities = List(nextEntity)),
      0.5
    ).value

    result.entities(previousEntity.id).speed shouldBe nextEntity.speed
    result.entities(previousEntity.id).shape shouldBe nextEntity.shape

  test("SceneInterpolator should interpolate world bounds"):
    val result = SceneInterpolator(
      stateWith(bounds = PreviousBounds),
      stateWith(bounds = NextBounds),
      0.5
    ).value

    result.bounds.lowerRight shouldBe Vector2D(150.0, 200.0)

  test("SceneInterpolator should use the next scene topology"):
    val previousEntity = Entity.circle("removed", Vector2D(0.0, 0.0), 1.0).value
    val nextEntity     = Entity.circle("added", Vector2D(10.0, 10.0), 1.0).value

    val result = SceneInterpolator(
      stateWith(entities = List(previousEntity)),
      stateWith(entities = List(nextEntity)),
      0.5
    ).value

    result.entities.keySet shouldBe Set(nextEntity.id)

  test("SceneInterpolator should return the next scene at alpha one"):
    val previousEntity = Entity.circle("entity", Vector2D(0.0, 0.0), 1.0).value
    val nextEntity     = previousEntity.moveTo(Vector2D(10.0, 15.0))
    val nextTeam       = Team.create("team").value

    val result = SceneInterpolator(
      stateWith(entities = List(previousEntity)),
      stateWith(entities = List(nextEntity), teams = List(nextTeam)),
      1.0
    ).value

    result.entities shouldBe Map(nextEntity.id -> nextEntity)
    result.teams shouldBe Map(nextTeam.id -> nextTeam)
