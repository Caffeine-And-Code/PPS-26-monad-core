package monad_core.simulator.infrastructure.engine.translators

import helpers.arrangers.MonadCoreSurfaceArranger.{
  BlueSurfaceId,
  DefaultAppliedForce,
  DefaultFrictionIndex,
  DefaultRectangleHeight,
  DefaultRectangleWidth,
  RedSurfaceId
}
import helpers.arrangers.{MonadCoreSurfaceArranger, ShapeKind}
import monad_core.engine.model.{Surface, Vector2D}
import monad_core.simulator.domain.engine.MonadCoreSurface
import monad_core.simulator.infrastructure.engine.translators.SurfaceTranslator.{
  toEngineModel,
  toSimulationSurface
}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SurfaceTranslatorTest extends AnyFunSuite with Matchers with Inside:

  val engineSurfaceWithOptionals: Surface =
    (for
      base <- Surface.circle(
        BlueSurfaceId,
        Vector2D(0, 0),
        radius = MonadCoreSurfaceArranger.DefaultCircleRadius
      )
      withFriction <- base.withFrictionIndex(DefaultFrictionIndex)
      withForce <- withFriction.withAppliedForce(
        Vector2D(DefaultAppliedForce._1, DefaultAppliedForce._2)
      )
    yield withForce).value

  test(
    "toSimulationSurface converts an engine circle surface without any optional field to a simulation surface correctly"
  ):
    val engineSurface = Surface
      .circle(RedSurfaceId, Vector2D(0, 0), radius = MonadCoreSurfaceArranger.DefaultCircleRadius)
      .value
    val expectedTranslation = MonadCoreSurfaceArranger.arrangeRedSurface(ShapeKind.Circle)

    val translationResult = engineSurface.toSimulationSurface

    translationResult should be(expectedTranslation)

  test(
    "toSimulationSurface converts an engine rectangle surface without any optional field to a simulation surface correctly"
  ):
    val engineSurface = Surface
      .rectangle(
        RedSurfaceId,
        Vector2D(0, 0),
        height = DefaultRectangleHeight,
        length = DefaultRectangleWidth
      )
      .value
    val expectedTranslation = MonadCoreSurfaceArranger.arrangeRedSurface(ShapeKind.Rectangle)

    val translationResult = engineSurface.toSimulationSurface

    translationResult should be(expectedTranslation)

  test(
    "toSimulationSurface converts an engine surface with optional fields to a simulation surface correctly"
  ):
    val expectedTranslation = MonadCoreSurfaceArranger.arrangeBlueSurface(ShapeKind.Circle)

    val translationResult = engineSurfaceWithOptionals.toSimulationSurface

    translationResult should be(expectedTranslation)

  test(
    "toEngineModel converts a valid simulation circle surface without optional fields to an engine surface in a correct way"
  ):
    val expectedEngineSurface = Surface
      .circle(RedSurfaceId, Vector2D(0, 0), radius = MonadCoreSurfaceArranger.DefaultCircleRadius)
      .value
    val simulationSurface = MonadCoreSurfaceArranger.arrangeRedSurface(ShapeKind.Circle)

    val translationResult = simulationSurface.toEngineModel

    inside(translationResult):
      case Right(surface) => surface should be(expectedEngineSurface)

  test(
    "toEngineModel converts a valid simulation rectangle surface without optional fields to an engine surface in a correct way"
  ):
    val expectedEngineSurface = Surface
      .rectangle(
        RedSurfaceId,
        Vector2D(0, 0),
        height = DefaultRectangleHeight,
        length = DefaultRectangleWidth
      )
      .value
    val simulationSurface = MonadCoreSurfaceArranger.arrangeRedSurface(ShapeKind.Rectangle)

    val translationResult = simulationSurface.toEngineModel

    inside(translationResult):
      case Right(surface) => surface should be(expectedEngineSurface)

  test(
    "toEngineModel converts a valid simulation surface with optional fields to an engine surface in a correct way"
  ):
    val expectedEngineSurface = engineSurfaceWithOptionals
    val simulationSurface     = MonadCoreSurfaceArranger.arrangeBlueSurface(ShapeKind.Circle)

    val translationResult = simulationSurface.toEngineModel

    inside(translationResult):
      case Right(surface) => surface should be(expectedEngineSurface)

  test("toEngineModel propagates the error when the shape is invalid"):
    val simulationSurface = MonadCoreSurface(
      id = RedSurfaceId,
      position = MonadCoreSurfaceArranger.DefaultPosition,
      shape = monad_core.simulator.domain.engine.MonadCoreShape.SimulationCircle(radius = -1.0)
    )

    val translationResult = simulationSurface.toEngineModel

    translationResult.isLeft should be(true)
