package monad_core.simulator.infrastructure.engine.translators.round_trips

import helpers.arrangers.MonadCoreEntityArranger
import helpers.arrangers.MonadCoreEntityArranger.*
import helpers.arrangers.ShapeKind.{Circle, Rectangle}
import monad_core.engine.model.{Entity, Vector2D}
import monad_core.simulator.infrastructure.engine.translators.EntityTranslator.{
  toEngineModel,
  toSimulationEntity
}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class EntityTranslatorTest extends AnyFunSuite with Matchers with Inside:

  test("EntityTranslator Round Trip property is respected for Engine Entities"):
    val baseCircle = Entity
      .circle(
        RedEntityId,
        Vector2D(DefaultPosition._1, DefaultPosition._2),
        DefaultCircleRadius
      )
      .value

    val baseRectangle = Entity
      .rectangle(
        RedEntityId,
        Vector2D(DefaultPosition._1, DefaultPosition._2),
        DefaultRectangleHeight,
        DefaultRectangleWidth
      )
      .value

    def completeEntity(entity: Entity): Entity =
      val either = for
        entityWithSpeed  = entity.withSpeed(Vector2D(10, 10))
        entityWithHealth <- entityWithSpeed.withHealth(11)
        entityWithWeight <- entityWithHealth.withWeight(15)
        complete         <- entityWithWeight.withTeamId("teamId")
      yield complete

      either.value

    val completeCircle    = completeEntity(baseCircle)
    val completeRectangle = completeEntity(baseRectangle)

    val possibleEntities = Table(
      "expectedEntity",
      baseCircle,
      baseRectangle,
      completeCircle,
      completeRectangle
    )

    forAll(possibleEntities): expectedEntity =>
      val translationResult = expectedEntity.toSimulationEntity.toEngineModel

      inside(translationResult):
        case Right(entity) =>
          entity should be(expectedEntity)

  test("EntityTranslator Round Trip property is respected for Simulator Entities"):
    val possibleEntities = Table(
      "expectedEntity",
      MonadCoreEntityArranger.arrangeRedEntity(Circle),
      MonadCoreEntityArranger.arrangeRedEntity(Circle, withOptionals = true),
      MonadCoreEntityArranger.arrangeRedEntity(Rectangle),
      MonadCoreEntityArranger.arrangeRedEntity(Rectangle, withOptionals = true)
    )

    forAll(possibleEntities): expectedEntity =>
      val translationResult = expectedEntity.toEngineModel
        .fold(
          error => fail(error.message),
          engineEntity => engineEntity.toSimulationEntity
        )

      translationResult should be(expectedEntity)
