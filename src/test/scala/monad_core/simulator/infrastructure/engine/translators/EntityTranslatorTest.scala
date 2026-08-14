package monad_core.simulator.infrastructure.engine.translators

import helpers.arrangers.MonadCoreEntityArranger.{
  BlueEntityId,
  DefaultHealth,
  DefaultRectangleHeight,
  DefaultRectangleWidth,
  DefaultSpeed,
  DefaultTeamId,
  DefaultWeight,
  RedEntityId
}
import helpers.arrangers.{MonadCoreEntityArranger, ShapeKind}
import monad_core.engine.model.{Entity, Vector2D}
import monad_core.simulator.domain.engine.MonadCoreEntity
import monad_core.simulator.infrastructure.engine.translators.EntityTranslator.{
  toEngineModel,
  toSimulationEntity
}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EntityTranslatorTest extends AnyFunSuite with Matchers with Inside:

  val engineEntityWithOptionals: Entity =
    (for
      base <- Entity.circle(
        BlueEntityId,
        Vector2D(0, 0),
        radius = MonadCoreEntityArranger.DefaultCircleRadius
      )
      withSpeed = base.withSpeed(Vector2D(DefaultSpeed._1, DefaultSpeed._2))
      withWeight <- withSpeed.withWeight(DefaultWeight)
      withHealth <- withWeight.withHealth(DefaultHealth)
      withTeam   <- withHealth.withTeamId(DefaultTeamId)
    yield withTeam).value

  test(
    "toSimulationEntity converts an engine circle entity without any optional field to a simulation entity correctly"
  ):
    val engineEntity = Entity
      .circle(RedEntityId, Vector2D(0, 0), radius = MonadCoreEntityArranger.DefaultCircleRadius)
      .value
    val expectedTranslation = MonadCoreEntityArranger.arrangeRedEntity(ShapeKind.Circle)

    val translationResult = engineEntity.toSimulationEntity

    translationResult should be(expectedTranslation)

  test(
    "toSimulationEntity converts an engine rectangle entity without any optional field to a simulation entity correctly"
  ):
    val engineEntity = Entity
      .rectangle(
        RedEntityId,
        Vector2D(0, 0),
        height = DefaultRectangleHeight,
        length = DefaultRectangleWidth
      )
      .value
    val expectedTranslation = MonadCoreEntityArranger.arrangeRedEntity(ShapeKind.Rectangle)

    val translationResult = engineEntity.toSimulationEntity

    translationResult should be(expectedTranslation)

  test(
    "toSimulationEntity converts an engine entity with optional fields to a simulation entity correctly"
  ):
    val expectedTranslation = MonadCoreEntityArranger.arrangeBlueEntity(ShapeKind.Circle)

    val translationResult = engineEntityWithOptionals.toSimulationEntity

    translationResult should be(expectedTranslation)

  test(
    "toEngineModel converts a valid simulation circle entity without optional fields to an engine entity in a correct way"
  ):
    val expectedEngineEntity = Entity
      .circle(RedEntityId, Vector2D(0, 0), radius = MonadCoreEntityArranger.DefaultCircleRadius)
      .value
    val simulationEntity = MonadCoreEntityArranger.arrangeRedEntity(ShapeKind.Circle)

    val translationResult = simulationEntity.toEngineModel

    inside(translationResult):
      case Right(entity) => entity should be(expectedEngineEntity)

  test(
    "toEngineModel converts a valid simulation rectangle entity without optional fields to an engine entity in a correct way"
  ):
    val expectedEngineEntity = Entity
      .rectangle(
        RedEntityId,
        Vector2D(0, 0),
        height = DefaultRectangleHeight,
        length = DefaultRectangleWidth
      )
      .value
    val simulationEntity = MonadCoreEntityArranger.arrangeRedEntity(ShapeKind.Rectangle)

    val translationResult = simulationEntity.toEngineModel

    inside(translationResult):
      case Right(entity) => entity should be(expectedEngineEntity)

  test(
    "toEngineModel converts a valid simulation entity with optional fields to an engine entity in a correct way"
  ):
    val expectedEngineEntity = engineEntityWithOptionals
    val simulationEntity     = MonadCoreEntityArranger.arrangeBlueEntity(ShapeKind.Circle)

    val translationResult = simulationEntity.toEngineModel

    inside(translationResult):
      case Right(entity) => entity should be(expectedEngineEntity)

  test("toEngineModel propagates the error when the shape is invalid"):
    val simulationEntity = MonadCoreEntity(
      id = RedEntityId,
      position = MonadCoreEntityArranger.DefaultPosition,
      shape = monad_core.simulator.domain.engine.MonadCoreShape.SimulationCircle(radius = -1.0)
    )

    val translationResult = simulationEntity.toEngineModel

    translationResult.isLeft should be(true)

  test("toEngineModel propagates the error when an optional field is invalid"):
    val simulationEntity =
      MonadCoreEntityArranger.arrangeRedEntity(ShapeKind.Circle).copy(weight = Some(-5))

    val translationResult = simulationEntity.toEngineModel

    translationResult.isLeft should be(true)
