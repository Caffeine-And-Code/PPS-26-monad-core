package monad_core.simulator.infrastructure.engine.translators.round_trips

import helpers.arrangers.MonadCoreSurfaceArranger
import helpers.arrangers.MonadCoreSurfaceArranger.*
import helpers.arrangers.ShapeKind.{Circle, Rectangle}
import monad_core.engine.model.{Surface, Vector2D}
import monad_core.simulator.infrastructure.engine.translators.SurfaceTranslator.{
  toEngineModel,
  toSimulationSurface
}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class SurfaceTranslatorTest extends AnyFunSuite with Matchers with Inside:

  test("SurfaceTranslator Round Trip property is respected for Engine Surfaces"):
    val baseCircle = Surface
      .circle(
        RedSurfaceId,
        Vector2D(DefaultPosition._1, DefaultPosition._2),
        DefaultCircleRadius
      )
      .value

    val baseRectangle = Surface
      .rectangle(
        RedSurfaceId,
        Vector2D(DefaultPosition._1, DefaultPosition._2),
        DefaultRectangleHeight,
        DefaultRectangleWidth
      )
      .value

    def completeSurface(surface: Surface): Surface =
      val either = for
        surfaceWithForce <- surface.withAppliedForce(Vector2D(10, 10))
        complete         <- surfaceWithForce.withFrictionIndex(9)
      yield complete

      either.value

    val completeCircle    = completeSurface(baseCircle)
    val completeRectangle = completeSurface(baseRectangle)

    val possibleEntities = Table(
      "expectedSurface",
      baseCircle,
      baseRectangle,
      completeCircle,
      completeRectangle
    )

    forAll(possibleEntities): expectedSurface =>
      val translationResult = expectedSurface.toSimulationSurface.toEngineModel

      inside(translationResult):
        case Right(entity) =>
          entity should be(expectedSurface)

  test("SurfaceTranslator Round Trip property is respected for Simulator Surfaces"):
    val possibleEntities = Table(
      "expectedSurface",
      MonadCoreSurfaceArranger.arrangeRedSurface(Circle),
      MonadCoreSurfaceArranger.arrangeRedSurface(Circle, withOptionals = true),
      MonadCoreSurfaceArranger.arrangeRedSurface(Rectangle),
      MonadCoreSurfaceArranger.arrangeRedSurface(Rectangle, withOptionals = true)
    )

    forAll(possibleEntities): expectedSurface =>
      val translationResult = expectedSurface.toEngineModel
        .fold(
          error => fail(error.message),
          engineEntity => engineEntity.toSimulationSurface
        )

      translationResult should be(expectedSurface)
