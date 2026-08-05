package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.*
import monad_core.engine.model.*
import monad_core.simulator.application.engine.world.{SaveEntityCommand, SaveSurfaceCommand, SaveTeamCommand, World}
import monad_core.simulator.domain.engine.{MonadCoreEntity, MonadCoreSurface}
import monad_core.simulator.domain.engine.MonadCoreShape.SimulationCircle
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.engine.MonadCoreWorld
import monad_core.simulator.infrastructure.engine.errors.EngineErrorAdapted
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class WorldTest extends AnyFunSuite with Matchers with MockFactory with Inside:

  val id : String = "id" 
  val radius : Double = 2
  val position : (Double, Double) = (0,0)
  
  val baseEntity: Entity = Entity.circle(id, Vector2D(position._1, position._2), radius).value
  val baseSurface: Surface = Surface.circle(id, Vector2D(position._1, position._2), radius).value
  val baseTeam: Team = Team(TeamId(id).value, Set.empty).value
  
  val baseSimulationEntity: MonadCoreEntity = MonadCoreEntity(id, position, SimulationCircle(radius))
  val baseSimulationSurface: MonadCoreSurface = MonadCoreSurface(id, position, SimulationCircle(radius))

  def newWorld(): (Scene, World) =
    val state: Scene = mock[Scene]
    (state, MonadCoreWorld(state))

  test("createEntity actually creates the entity"):
    val (state, worldTest) = newWorld()
    val nextState: Scene = mock[Scene]
    state.addEntity.expects(baseEntity).returns(Right(nextState))
    val command = SaveEntityCommand(baseSimulationEntity)

    val result = worldTest.createEntity(command)

    result shouldBe Right(())
    worldTest.scene shouldBe nextState

  test("createEntity carries the state errors"):
    val (state, worldTest) = newWorld()
    val expectedError = CannotAddEntity(CannotAddAlreadyPresentElementInMap(baseEntity.id))
    state.addEntity.expects(baseEntity).returns(Left(expectedError))
    val command = SaveEntityCommand(baseSimulationEntity)

    val result = worldTest.createEntity(command)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))

  test("removeEntity removes entity successfully when present"):
    val (state, worldTest) = newWorld()
    val nextState = mock[Scene]
    state.getEntity.expects(baseEntity.id).returns(Right(baseEntity))
    state.removeEntity.expects(baseEntity).returns(Right(nextState))

    val result = worldTest.removeEntity(baseSimulationEntity.id)

    result shouldBe Right(())
    worldTest.scene shouldBe nextState

  test("removeEntity propagates error if entity is not found"):
    val (state, worldTest) = newWorld()
    val expectedError = EntityNotFound(baseEntity.id)
    state.getEntity.expects(baseEntity.id).returns(Left(expectedError))

    val result = worldTest.removeEntity(baseSimulationEntity.id)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))

  test("updateEntity removes old entity and creates updated one"):
    val (state, worldTest) = newWorld()
    val intermediateState = mock[Scene]
    val finalState = mock[Scene]
    val command = SaveEntityCommand(baseSimulationEntity)

    state.getEntity.expects(baseEntity.id).returns(Right(baseEntity))
    state.removeEntity.expects(baseEntity).returns(Right(intermediateState))
    intermediateState.addEntity.expects(baseEntity).returns(Right(finalState))
    finalState.getEntity.expects(baseEntity.id).returns(Right(baseEntity))

    val result = worldTest.updateEntity(command)

    result shouldBe Right(())
    worldTest.scene shouldBe finalState
    inside(worldTest.getEntity(baseSimulationEntity.id)):
      case Right(fetchedEntity) => fetchedEntity should be(baseSimulationEntity)

  test("updateEntity propagates the state error if Entity was not found"):
    val (state, worldTest) = newWorld()
    val command = SaveEntityCommand(baseSimulationEntity)
    val expectedError = EntityNotFound(baseEntity.id)

    state.getEntity.expects(baseEntity.id).returns(Left(expectedError))

    val result = worldTest.updateEntity(command)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))

  test("createTeam actually creates the team"):
    val (state, worldTest) = newWorld()
    val nextState: Scene = mock[Scene]
    state.addTeam.expects(baseTeam).returns(Right(nextState))
    val command = SaveTeamCommand(baseTeam)

    val result = worldTest.createTeam(command)

    result shouldBe Right(())
    worldTest.scene shouldBe nextState

  test("createTeam carries the state errors"):
    val (state, worldTest) = newWorld()
    val expectedError = CannotAddTeam(CannotAddAlreadyPresentElementInMap(baseTeam.id))
    state.addTeam.expects(baseTeam).returns(Left(expectedError))
    val command = SaveTeamCommand(baseTeam)

    val result = worldTest.createTeam(command)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))

  test("removeTeam removes team successfully when present"):
    val (state, worldTest) = newWorld()
    val nextState = mock[Scene]
    state.getTeam.expects(baseTeam.id).returns(Right(baseTeam))
    state.removeTeam.expects(baseTeam).returns(Right(nextState))

    val result = worldTest.removeTeam(baseTeam.id)

    result shouldBe Right(())
    worldTest.scene shouldBe nextState

  test("removeTeam propagates error if team is not found"):
    val (state, worldTest) = newWorld()
    val expectedError = TeamNotFound(baseTeam.id)
    state.getTeam.expects(baseTeam.id).returns(Left(expectedError))

    val result = worldTest.removeTeam(baseTeam.id)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))

  test("updateTeam removes old team and creates updated one"):
    val (state, worldTest) = newWorld()
    val intermediateState = mock[Scene]
    val finalState = mock[Scene]
    val command = SaveTeamCommand(baseTeam)

    state.getTeam.expects(baseTeam.id).returns(Right(baseTeam))
    state.removeTeam.expects(baseTeam).returns(Right(intermediateState))
    intermediateState.addTeam.expects(baseTeam).returns(Right(finalState))
    finalState.getTeam.expects(baseTeam.id).returns(Right(baseTeam))

    val result = worldTest.updateTeam(command)

    result shouldBe Right(())
    worldTest.scene shouldBe finalState
    inside(worldTest.getTeam(baseTeam.id)):
      case Right(fetchedTeam) => fetchedTeam should be(baseTeam)

  test("updateTeam propagates the state error if Team was not found"):
    val (state, worldTest) = newWorld()
    val command = SaveTeamCommand(baseTeam)
    val expectedError = TeamNotFound(baseTeam.id)

    state.getTeam.expects(baseTeam.id).returns(Left(expectedError))

    val result = worldTest.updateTeam(command)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))

  test("createSurface actually creates the surface"):
    val (state, worldTest) = newWorld()
    val nextState: Scene = mock[Scene]

    state.addSurface.expects(baseSurface).returns(Right(nextState))
    val command = SaveSurfaceCommand(baseSimulationSurface)

    val result = worldTest.createSurface(command)

    result shouldBe Right(())
    worldTest.scene shouldBe nextState

  test("createSurface carries the state errors"):
    val (state, worldTest) = newWorld()
    val expectedError = CannotAddSurface(CannotAddAlreadyPresentElementInMap(baseSurface.id))
    state.addSurface.expects(baseSurface).returns(Left(expectedError))
    val command = SaveSurfaceCommand(baseSimulationSurface)

    val result = worldTest.createSurface(command)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))

  test("removeSurface removes surface successfully when present"):
    val (state, worldTest) = newWorld()
    val nextState = mock[Scene]
    state.getSurface.expects(baseSurface.id).returns(Right(baseSurface))
    state.removeSurface.expects(baseSurface).returns(Right(nextState))

    val result = worldTest.removeSurface(baseSimulationSurface.id)

    result shouldBe Right(())
    worldTest.scene shouldBe nextState

  test("removeSurface propagates error if surface is not found"):
    val (state, worldTest) = newWorld()
    val expectedError = SurfaceNotFound(baseSurface.id)
    state.getSurface.expects(baseSurface.id).returns(Left(expectedError))

    val result = worldTest.removeSurface(baseSimulationSurface.id)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))

  test("updateSurface removes old surface and creates updated one"):
    val (state, worldTest) = newWorld()
    val intermediateState = mock[Scene]
    val finalState = mock[Scene]
    val command = SaveSurfaceCommand(baseSimulationSurface)

    state.getSurface.expects(baseSurface.id).returns(Right(baseSurface))
    state.removeSurface.expects(baseSurface).returns(Right(intermediateState))
    intermediateState.addSurface.expects(baseSurface).returns(Right(finalState))
    finalState.getSurface.expects(baseSurface.id).returns(Right(baseSurface))

    val result = worldTest.updateSurface(command)

    result shouldBe Right(())
    worldTest.scene shouldBe finalState
    inside(worldTest.getSurface(baseSimulationSurface.id)):
      case Right(fetchedSurface) => fetchedSurface should be(baseSimulationSurface)

  test("updateSurface propagates the state error if Surface was not found"):
    val (state, worldTest) = newWorld()
    val command = SaveSurfaceCommand(baseSimulationSurface)
    val expectedError = SurfaceNotFound(baseSurface.id)

    state.getSurface.expects(baseSurface.id).returns(Left(expectedError))

    val result = worldTest.updateSurface(command)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))