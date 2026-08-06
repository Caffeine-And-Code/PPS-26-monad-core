package monad_core.simulator.infrastructure.engine.translators

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{LocatableIdCannotBeEmpty, Shape2D, Vector2D, Entity as EngineEntity, Surface as EngineSurface, Team as EngineTeam}
import monad_core.simulator.domain.engine.MonadCoreShape.{SimulationCircle, SimulationRectangle}
import monad_core.simulator.infrastructure.engine.translators.BaseTranslator.{toSimulationEitherEntity, toSimulationEitherSurface, toSimulationEitherTeam}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BaseTranslatorTest extends AnyFunSuite with Matchers with Inside:

  test("determineShape maps a Circle preserving the radius"):
    val expectedRadius: Double = 5.0
    val engineCircle = Shape2D.circle(expectedRadius).value

    val resultShape = BaseTranslator.determineShape(engineCircle)

    resultShape shouldBe SimulationCircle(expectedRadius)

  test("determineShape maps a Rectangle: width from engine length, height from engine height"):
    val expectedWidth: Double = 7.0
    val expectedHeight: Double = 3.0
    val engineRectangle = Shape2D.rectangle(height = expectedHeight, length = expectedWidth).value

    val resultShape = BaseTranslator.determineShape(engineRectangle)

    resultShape shouldBe SimulationRectangle(width = expectedWidth, height = expectedHeight)

  test("toSimulationEitherEntity maps the entity fields when Right"):
    val expectedId: String = "e1"
    val expectedPosition: (Double, Double) = (1.0, 2.0)
    val expectedRadius: Double = 3.0
    val entity = EngineEntity.circle(expectedId, Vector2D(expectedPosition._1, expectedPosition._2), radius = expectedRadius).value
    val either: Either[EngineError, EngineEntity] = Right(entity)

    val result = either.toSimulationEitherEntity

    inside(result):
      case Right(simulationEntity) =>
        simulationEntity.id shouldBe expectedId
        simulationEntity.position shouldBe expectedPosition
        simulationEntity.shape shouldBe SimulationCircle(expectedRadius)

  test("toSimulationEitherEntity propagates the error when Left"):
    val error: EngineError = LocatableIdCannotBeEmpty()
    val either: Either[EngineError, EngineEntity] = Left(error)

    val result = either.toSimulationEitherEntity

    inside(result):
      case Left(actualError) => actualError shouldBe error

  test("toSimulationEitherSurface maps the surface fields when Right"):
    val expectedId: String = "s1"
    val expectedPosition: (Double, Double) = (1.0, 2.0)
    val expectedRadius: Double = 3.0
    val surface = EngineSurface.circle(expectedId, Vector2D(expectedPosition._1, expectedPosition._2), radius = expectedRadius).value
    val either: Either[EngineError, EngineSurface] = Right(surface)

    val result = either.toSimulationEitherSurface

    inside(result):
      case Right(simulationSurface) =>
        simulationSurface.id shouldBe expectedId
        simulationSurface.position shouldBe expectedPosition
        simulationSurface.shape shouldBe SimulationCircle(expectedRadius)

  test("toSimulationEitherSurface propagates the error when Left"):
    val error: EngineError = LocatableIdCannotBeEmpty()
    val either: Either[EngineError, EngineSurface] = Left(error)

    val result = either.toSimulationEitherSurface

    inside(result):
      case Left(actualError) => actualError shouldBe error

  test("toSimulationEitherTeam maps the team fields when Right"):
    val teamId: String = "t1"
    val enemies: Set[String] = Set("t2")
    val team = EngineTeam.create(teamId, enemies).value
    val either: Either[EngineError, EngineTeam] = Right(team)

    val result = either.toSimulationEitherTeam

    inside(result):
      case Right(simulationTeam) =>
        simulationTeam.id shouldBe teamId
        simulationTeam.enemies shouldBe enemies

  test("toSimulationEitherTeam propagates the error when Left"):
    val error: EngineError = LocatableIdCannotBeEmpty()
    val either: Either[EngineError, EngineTeam] = Left(error)

    val result = either.toSimulationEitherTeam

    inside(result):
      case Left(actualError) => actualError shouldBe error