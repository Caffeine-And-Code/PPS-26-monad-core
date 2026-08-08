package monad_core.simulator.infrastructure.engine.translators.round_trips

import helpers.arrangers.MonadCoreEntityArranger.{BlueEntityId, RedEntityId}
import helpers.arrangers.MonadCoreSurfaceArranger.{BlueSurfaceId, RedSurfaceId}
import helpers.arrangers.MonadCoreTeamArranger.{BlueTeamId, RedTeamId}
import helpers.arrangers.ShapeKind.{Circle, Rectangle}
import helpers.arrangers.{MonadCoreEntityArranger, MonadCoreSurfaceArranger, MonadCoreTeamArranger, ShapeKind}
import monad_core.engine.core.Scene
import monad_core.engine.model.{Entity, LocatableId, Surface, Team, TeamId, Vector2D}
import monad_core.simulator.domain.engine.MonadCoreScene
import monad_core.simulator.infrastructure.engine.translators.SceneTranslator.{toEngineModel, toSimulationScene}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class SceneTranslatorTest extends AnyFunSuite with Matchers with Inside:

  private def entity(id: String, shapeKind: ShapeKind, withOptionals: Boolean): Entity =
    val base = shapeKind match
      case Circle => Entity.circle(id, Vector2D(0, 0), radius = MonadCoreEntityArranger.DefaultCircleRadius)
      case Rectangle => Entity.rectangle(id, Vector2D(0, 0), height = MonadCoreEntityArranger.DefaultRectangleHeight, length = MonadCoreEntityArranger.DefaultRectangleWidth)

    val either =
      if withOptionals then
        for
          e <- base
          withSpeed <- e.withSpeed(Vector2D(MonadCoreEntityArranger.DefaultSpeed._1, MonadCoreEntityArranger.DefaultSpeed._2))
          withWeight <- withSpeed.withWeight(MonadCoreEntityArranger.DefaultWeight)
          withHealth <- withWeight.withHealth(MonadCoreEntityArranger.DefaultHealth)
          withTeam <- withHealth.withTeamId(MonadCoreEntityArranger.DefaultTeamId)
        yield withTeam
      else base

    either.value

  private def surface(id: String, shapeKind: ShapeKind, withOptionals: Boolean): Surface =
    val base = shapeKind match
      case Circle => Surface.circle(id, Vector2D(0, 0), radius = MonadCoreSurfaceArranger.DefaultCircleRadius)
      case Rectangle => Surface.rectangle(id, Vector2D(0, 0), height = MonadCoreSurfaceArranger.DefaultRectangleHeight, length = MonadCoreSurfaceArranger.DefaultRectangleWidth)

    val either =
      if withOptionals then
        for
          s <- base
          withFriction <- s.withFrictionIndex(MonadCoreSurfaceArranger.DefaultFrictionIndex)
          withForce <- withFriction.withAppliedForce(Vector2D(MonadCoreSurfaceArranger.DefaultAppliedForce._1, MonadCoreSurfaceArranger.DefaultAppliedForce._2))
        yield withForce
      else base

    either.value

  private def team(id: String, enemies: Set[String]): Team =
    Team.create(id, enemies).value

  test("SceneTranslator Round Trip property is respected for Engine Scenes"):
    val emptyScene = Scene()
    val entityId = LocatableId(RedEntityId).value
    val teamId = TeamId(RedTeamId).value
    val surfaceId = LocatableId(RedSurfaceId).value

    val redOnlyScene = Scene(
      entities = Map(entityId -> entity(RedEntityId, Circle, withOptionals = false)),
      teams = Map(teamId -> team(RedTeamId, Set.empty)),
      surfaces = Map(surfaceId -> surface(RedSurfaceId, Circle, withOptionals = false))
    )

    val fullScene = Scene(
      entities = Map(
        entityId -> entity(RedEntityId, Rectangle, withOptionals = false),
        LocatableId(BlueEntityId).value -> entity(BlueEntityId, Circle, withOptionals = true)
      ),
      teams = Map(
        teamId -> team(RedTeamId, Set(BlueTeamId)),
        TeamId(BlueTeamId).value -> team(BlueTeamId, Set.empty)
      ),
      surfaces = Map(
        surfaceId -> surface(RedSurfaceId, Rectangle, withOptionals = false),
        LocatableId(BlueSurfaceId).value -> surface(BlueSurfaceId, Circle, withOptionals = true)
      )
    )

    val possibleScenes = Table(
      "expectedScene",
      emptyScene,
      redOnlyScene,
      fullScene
    )

    forAll(possibleScenes): expectedScene =>
      val translationResult = expectedScene.toSimulationScene.toEngineModel

      inside(translationResult):
        case Right(scene) =>
          scene should be(expectedScene)

  test("SceneTranslator Round Trip property is respected for Simulator Scenes"):
    val emptyScene = MonadCoreScene()

    val redOnlyScene = MonadCoreScene(
      entities = List(MonadCoreEntityArranger.arrangeRedEntity(Circle)),
      teams = List(MonadCoreTeamArranger.arrangeRedTeamWithoutEnemies),
      surfaces = List(MonadCoreSurfaceArranger.arrangeRedSurface(Circle))
    )

    val blueWithOptionalsScene = MonadCoreScene(
      entities = List(MonadCoreEntityArranger.arrangeBlueEntity(Rectangle)),
      teams = List(MonadCoreTeamArranger.arrangeBlueTeamWithoutEnemies),
      surfaces = List(MonadCoreSurfaceArranger.arrangeBlueSurface(Rectangle))
    )

    val possibleScenes = Table(
      "expectedScene",
      emptyScene,
      redOnlyScene,
      blueWithOptionalsScene
    )

    forAll(possibleScenes): expectedScene =>
      val translationResult = expectedScene
        .toEngineModel
        .fold(
          error => fail(error.message),
          engineScene => engineScene.toSimulationScene
        )

      translationResult should be(expectedScene)