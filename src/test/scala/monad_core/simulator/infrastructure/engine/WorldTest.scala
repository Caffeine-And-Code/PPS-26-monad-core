package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.*
import monad_core.engine.model.*
import monad_core.simulator.application.engine.world.{
  SaveEntityCommand,
  SaveSurfaceCommand,
  SaveTeamCommand
}
import monad_core.simulator.domain.engine.MonadCoreShape.SimulationCircle
import monad_core.simulator.domain.engine.{
  MonadCoreEntity,
  MonadCoreScene,
  MonadCoreSurface,
  MonadCoreTeam
}
import monad_core.simulator.infrastructure.engine.errors.EngineErrorAdapted
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class WorldTest extends AnyFunSuite with Matchers with Inside:

  val id: String                 = "id"
  val radius: Double             = 2
  val updatedRadius: Double      = 5
  val position: (Double, Double) = (0, 0)

  val engineId: LocatableId = LocatableId(id).value

  val baseSimulationEntity: MonadCoreEntity =
    MonadCoreEntity(id, position, SimulationCircle(radius))

  val updatedSimulationEntity: MonadCoreEntity =
    MonadCoreEntity(id, position, SimulationCircle(updatedRadius))

  val baseSimulationSurface: MonadCoreSurface =
    MonadCoreSurface(id, position, SimulationCircle(radius))

  val updatedSimulationSurface: MonadCoreSurface =
    MonadCoreSurface(id, position, SimulationCircle(updatedRadius))

  val baseSimulationTeam: MonadCoreTeam = MonadCoreTeam(id, Set.empty)

  def newWorld(initialScene: MonadCoreScene = MonadCoreScene()): MonadCoreWorld =
    MonadCoreWorld(initialScene)

  test("createEntity actually creates the entity"):
    val worldTest = newWorld()
    val command   = SaveEntityCommand(baseSimulationEntity)

    val result = worldTest.createEntity(command)

    result shouldBe Right(())
    inside(worldTest.getEntity(id)):
      case Right(entity) => entity should be(baseSimulationEntity)

  test("createEntity carries the state errors"):
    val worldTest     = newWorld(MonadCoreScene(entities = List(baseSimulationEntity)))
    val expectedError = CannotAddEntity(CannotAddAlreadyPresentElementInMap(engineId))
    val command       = SaveEntityCommand(baseSimulationEntity)

    val result = worldTest.createEntity(command)

    inside(result):
      case Left(error) => error should be(EngineErrorAdapted(expectedError))

  test("removeEntity removes entity successfully when present"):
    val worldTest = newWorld(MonadCoreScene(entities = List(baseSimulationEntity)))

    val result = worldTest.removeEntity(id)

    result shouldBe Right(())
    inside(worldTest.getEntity(id)):
      case Left(error) => error should be(EngineErrorAdapted(EntityNotFound(engineId)))

  test("removeEntity propagates error if entity is not found"):
    val worldTest     = newWorld()
    val expectedError = EntityNotFound(engineId)

    val result = worldTest.removeEntity(id)

    inside(result):
      case Left(error) => error should be(EngineErrorAdapted(expectedError))

  test("updateEntity removes old entity and creates updated one"):
    val worldTest = newWorld(MonadCoreScene(entities = List(baseSimulationEntity)))
    val command   = SaveEntityCommand(updatedSimulationEntity)

    val result = worldTest.updateEntity(command)

    result shouldBe Right(())
    inside(worldTest.getEntity(id)):
      case Right(entity) => entity should be(updatedSimulationEntity)

  test("updateEntity propagates the state error if Entity was not found"):
    val worldTest     = newWorld()
    val command       = SaveEntityCommand(baseSimulationEntity)
    val expectedError = EntityNotFound(engineId)

    val result = worldTest.updateEntity(command)

    inside(result):
      case Left(error) => error should be(EngineErrorAdapted(expectedError))

  test("createTeam actually creates the team"):
    val worldTest = newWorld()
    val command   = SaveTeamCommand(baseSimulationTeam)

    val result = worldTest.createTeam(command)

    result shouldBe Right(())
    inside(worldTest.getTeam(id)):
      case Right(team) => team should be(baseSimulationTeam)

  test("createTeam carries the state errors"):
    val worldTest     = newWorld(MonadCoreScene(teams = List(baseSimulationTeam)))
    val expectedError = CannotAddTeam(CannotAddAlreadyPresentElementInMap(TeamId(id).value))
    val command       = SaveTeamCommand(baseSimulationTeam)

    val result = worldTest.createTeam(command)

    inside(result):
      case Left(error) => error should be(EngineErrorAdapted(expectedError))

  test("removeTeam removes team successfully when present"):
    val worldTest = newWorld(MonadCoreScene(teams = List(baseSimulationTeam)))

    val result = worldTest.removeTeam(id)

    result shouldBe Right(())
    inside(worldTest.getTeam(id)):
      case Left(error) => error should be(EngineErrorAdapted(TeamNotFound(TeamId(id).value)))

  test("removeTeam propagates error if team is not found"):
    val worldTest     = newWorld()
    val expectedError = TeamNotFound(TeamId(id).value)

    val result = worldTest.removeTeam(id)

    inside(result):
      case Left(error) => error should be(EngineErrorAdapted(expectedError))

  test("updateTeam removes old team and creates updated one"):
    val worldTest   = newWorld(MonadCoreScene(teams = List(baseSimulationTeam)))
    val updatedTeam = MonadCoreTeam(id, Set.empty)
    val command     = SaveTeamCommand(updatedTeam)

    val result = worldTest.updateTeam(command)

    result shouldBe Right(())
    inside(worldTest.getTeam(id)):
      case Right(team) => team should be(updatedTeam)

  test("updateTeam propagates the state error if Team was not found"):
    val worldTest     = newWorld()
    val command       = SaveTeamCommand(baseSimulationTeam)
    val expectedError = TeamNotFound(TeamId(id).value)

    val result = worldTest.updateTeam(command)

    inside(result):
      case Left(error) => error should be(EngineErrorAdapted(expectedError))

  test("createSurface actually creates the surface"):
    val worldTest = newWorld()
    val command   = SaveSurfaceCommand(baseSimulationSurface)

    val result = worldTest.createSurface(command)

    result shouldBe Right(())
    inside(worldTest.getSurface(id)):
      case Right(surface) => surface should be(baseSimulationSurface)

  test("createSurface carries the state errors"):
    val worldTest     = newWorld(MonadCoreScene(surfaces = List(baseSimulationSurface)))
    val expectedError = CannotAddSurface(CannotAddAlreadyPresentElementInMap(engineId))
    val command       = SaveSurfaceCommand(baseSimulationSurface)

    val result = worldTest.createSurface(command)

    inside(result):
      case Left(error) => error should be(EngineErrorAdapted(expectedError))

  test("removeSurface removes surface successfully when present"):
    val worldTest = newWorld(MonadCoreScene(surfaces = List(baseSimulationSurface)))

    val result = worldTest.removeSurface(id)

    result shouldBe Right(())
    inside(worldTest.getSurface(id)):
      case Left(error) => error should be(EngineErrorAdapted(SurfaceNotFound(engineId)))

  test("removeSurface propagates error if surface is not found"):
    val worldTest     = newWorld()
    val expectedError = SurfaceNotFound(engineId)

    val result = worldTest.removeSurface(id)

    inside(result):
      case Left(error) => error should be(EngineErrorAdapted(expectedError))

  test("updateSurface removes old surface and creates updated one"):
    val worldTest = newWorld(MonadCoreScene(surfaces = List(baseSimulationSurface)))
    val command   = SaveSurfaceCommand(updatedSimulationSurface)

    val result = worldTest.updateSurface(command)

    result shouldBe Right(())
    inside(worldTest.getSurface(id)):
      case Right(surface) => surface should be(updatedSimulationSurface)

  test("updateSurface propagates the state error if Surface was not found"):
    val worldTest     = newWorld()
    val command       = SaveSurfaceCommand(baseSimulationSurface)
    val expectedError = SurfaceNotFound(engineId)

    val result = worldTest.updateSurface(command)

    inside(result):
      case Left(error) => error should be(EngineErrorAdapted(expectedError))
