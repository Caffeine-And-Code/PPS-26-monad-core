package monad_core.simulator.infrastructure.engine.translators

import helpers.arrangers.MonadCoreEntityArranger.RedEntityId
import helpers.arrangers.MonadCoreSurfaceArranger.RedSurfaceId
import helpers.arrangers.MonadCoreTeamArranger.RedTeamId
import helpers.arrangers.{MonadCoreEntityArranger, MonadCoreSurfaceArranger, MonadCoreTeamArranger, ShapeKind}
import monad_core.engine.core.Scene
import monad_core.engine.model.{Entity, Surface, Team, Vector2D}
import monad_core.simulator.domain.engine.{MonadCoreEntity, MonadCoreScene, MonadCoreShape, MonadCoreSurface, MonadCoreTeam}
import monad_core.simulator.infrastructure.engine.translators.SceneTranslator.{toEngineModel, toSimulationScene}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SceneTranslatorTest extends AnyFunSuite with Matchers with Inside:

  val engineEntity: Entity = Entity.circle(RedEntityId, Vector2D(0, 0), radius = MonadCoreEntityArranger.DefaultCircleRadius).value
  val engineTeam: Team = Team.create(RedTeamId).value
  val engineSurface: Surface = Surface.circle(RedSurfaceId, Vector2D(0, 0), radius = MonadCoreSurfaceArranger.DefaultCircleRadius).value

  val engineSceneWithData: Scene = Scene(
    entities = Map(engineEntity.id -> engineEntity),
    teams = Map(engineTeam.id -> engineTeam),
    surfaces = Map(engineSurface.id -> engineSurface)
  )

  val simulationSceneWithData: MonadCoreScene = MonadCoreScene(
    entities = List(MonadCoreEntityArranger.arrangeRedEntity(ShapeKind.Circle)),
    teams = List(MonadCoreTeamArranger.arrangeRedTeamWithoutEnemies),
    surfaces = List(MonadCoreSurfaceArranger.arrangeRedSurface(ShapeKind.Circle))
  )

  test("toSimulationScene converts an empty engine scene to an empty simulation scene correctly"):
    val engineScene = Scene()
    val expectedTranslation = MonadCoreScene()

    val translationResult = engineScene.toSimulationScene

    translationResult should be(expectedTranslation)

  test("toSimulationScene converts an engine scene with entities, teams and surfaces to a simulation scene correctly"):
    val translationResult = engineSceneWithData.toSimulationScene

    translationResult should be(simulationSceneWithData)

  test("toEngineModel converts a valid empty simulation scene to an engine scene in a correct way"):
    val simulationScene = MonadCoreScene()
    val expectedEngineScene = Scene()

    val translationResult = simulationScene.toEngineModel

    inside(translationResult):
      case Right(scene) => scene should be(expectedEngineScene)

  test("toEngineModel converts a valid simulation scene with entities, teams and surfaces to an engine scene in a correct way"):
    val translationResult = simulationSceneWithData.toEngineModel

    inside(translationResult):
      case Right(scene) => scene should be(engineSceneWithData)

  test("toEngineModel propagates the error when an entity is invalid"):
    val invalidEntity = MonadCoreEntity(
      id = RedEntityId,
      position = MonadCoreEntityArranger.DefaultPosition,
      shape = MonadCoreShape.SimulationCircle(radius = -1.0)
    )
    val simulationScene = MonadCoreScene(entities = List(invalidEntity))

    val translationResult = simulationScene.toEngineModel

    translationResult.isLeft should be(true)

  test("toEngineModel propagates the error when a team is invalid"):
    val invalidTeam = MonadCoreTeam(id = RedTeamId, enemies = Set(RedTeamId))
    val simulationScene = MonadCoreScene(teams = List(invalidTeam))

    val translationResult = simulationScene.toEngineModel

    translationResult.isLeft should be(true)

  test("toEngineModel propagates the error when a surface is invalid"):
    val invalidSurface = MonadCoreSurface(
      id = RedSurfaceId,
      position = MonadCoreSurfaceArranger.DefaultPosition,
      shape = MonadCoreShape.SimulationCircle(radius = -1.0)
    )
    val simulationScene = MonadCoreScene(surfaces = List(invalidSurface))

    val translationResult = simulationScene.toEngineModel

    translationResult.isLeft should be(true)