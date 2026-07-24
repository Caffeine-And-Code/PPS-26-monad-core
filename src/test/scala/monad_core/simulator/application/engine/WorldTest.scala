package monad_core.simulator.application.engine

import monad_core.engine.core.{CannotAddAlreadyPresentElementInMap, CannotAddEntity, CannotAddSurface, CannotAddTeam, EntityNotFound, SurfaceNotFound, TeamNotFound}
import monad_core.engine.core.traits.State
import monad_core.engine.model.{Entity, Surface, Team, TeamId, Vector2D}
import monad_core.simulator.application.engine.world.{SaveEntityCommand, SaveSurfaceCommand, SaveTeamCommand, World}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class WorldTest extends AnyFunSuite with Matchers with MockFactory with Inside:
  val state: State = mock[State]
  val baseEntity: Entity = Entity.circle("id", Vector2D(0, 0), 2).value
  val baseSurface: Surface = Surface.circle("id", Vector2D(0, 0), 2).value
  val baseTeam: Team = Team(TeamId("id").value, Set.empty).value
  val worldTest: World = World(state)

  test("createEntity actually creates the entity"):
    val nextState: State = mock[State]
    state.addEntity.expects(baseEntity).returns(Right(nextState))
    val command = SaveEntityCommand(baseEntity)

    val result = worldTest.createEntity(command)

    inside(result):
      case Right(world) =>
        world.snapshot should be(nextState)

  test("createEntity carries the state errors"):
    val expectedError = CannotAddEntity(CannotAddAlreadyPresentElementInMap(baseEntity.id))
    state.addEntity.expects(baseEntity).returns(Left(expectedError))
    val command = SaveEntityCommand(baseEntity)

    val result = worldTest.createEntity(command)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

  test("removeEntity removes entity successfully when present"):
    val nextState = mock[State]
    state.getEntity.expects(baseEntity.id).returns(Right(baseEntity))
    state.removeEntity.expects(baseEntity).returns(Right(nextState))

    val result = worldTest.removeEntity(baseEntity.id)

    inside(result):
      case Right(world) =>
        world.snapshot should be(nextState)

  test("removeEntity propagates error if entity is not found"):
    val expectedError = EntityNotFound(baseEntity.id)
    state.getEntity.expects(baseEntity.id).returns(Left(expectedError))

    val result = worldTest.removeEntity(baseEntity.id)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

  test("updateEntity removes old entity and creates updated one"):
    val intermediateState = mock[State]
    val finalState = mock[State]
    val command = SaveEntityCommand(baseEntity)

    state.getEntity.expects(baseEntity.id).returns(Right(baseEntity))
    state.removeEntity.expects(baseEntity).returns(Right(intermediateState))
    intermediateState.addEntity.expects(baseEntity).returns(Right(finalState))

    val result = worldTest.updateEntity(command)

    inside(result):
      case Right(world) =>
        world.snapshot should be(finalState)

  test("updateEntity propagates the state error if Entity was not found"):
    val command = SaveEntityCommand(baseEntity)
    val expectedError = EntityNotFound(baseEntity.id)

    state.getEntity.expects(baseEntity.id).returns(Left(expectedError))

    val result = worldTest.updateEntity(command)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

  test("createTeam actually creates the team"):
    val nextState: State = mock[State]
    state.addTeam.expects(baseTeam).returns(Right(nextState))
    val command = SaveTeamCommand(baseTeam)

    val result = worldTest.createTeam(command)

    inside(result):
      case Right(world) =>
        world.snapshot should be(nextState)

  test("createTeam carries the state errors"):
    val expectedError = CannotAddTeam(CannotAddAlreadyPresentElementInMap(baseTeam.id))
    state.addTeam.expects(baseTeam).returns(Left(expectedError))
    val command = SaveTeamCommand(baseTeam)

    val result = worldTest.createTeam(command)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

  test("removeTeam removes team successfully when present"):
    val nextState = mock[State]
    state.getTeam.expects(baseTeam.id).returns(Right(baseTeam))
    state.removeTeam.expects(baseTeam).returns(Right(nextState))

    val result = worldTest.removeTeam(baseTeam.id)

    inside(result):
      case Right(world) =>
        world.snapshot should be(nextState)

  test("removeTeam propagates error if team is not found"):
    val expectedError = TeamNotFound(baseTeam.id)
    state.getTeam.expects(baseTeam.id).returns(Left(expectedError))

    val result = worldTest.removeTeam(baseTeam.id)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

  test("updateTeam removes old team and creates updated one"):
    val intermediateState = mock[State]
    val finalState = mock[State]
    val command = SaveTeamCommand(baseTeam)

    state.getTeam.expects(baseTeam.id).returns(Right(baseTeam))
    state.removeTeam.expects(baseTeam).returns(Right(intermediateState))
    intermediateState.addTeam.expects(baseTeam).returns(Right(finalState))

    val result = worldTest.updateTeam(command)

    inside(result):
      case Right(world) =>
        world.snapshot should be(finalState)

  test("updateTeam propagates the state error if Team was not found"):
    val command = SaveTeamCommand(baseTeam)
    val expectedError = TeamNotFound(baseTeam.id)

    state.getTeam.expects(baseTeam.id).returns(Left(expectedError))

    val result = worldTest.updateTeam(command)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

  test("createSurface actually creates the surface"):
    val nextState: State = mock[State]

    state.addSurface.expects(baseSurface).returns(Right(nextState))
    val command = SaveSurfaceCommand(baseSurface)

    val result = worldTest.createSurface(command)

    inside(result):
      case Right(world) =>
        world.snapshot should be(nextState)

  test("createSurface carries the state errors"):
    val expectedError = CannotAddSurface(CannotAddAlreadyPresentElementInMap(baseSurface.id))
    state.addSurface.expects(baseSurface).returns(Left(expectedError))
    val command = SaveSurfaceCommand(baseSurface)

    val result = worldTest.createSurface(command)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

  test("removeSurface removes surface successfully when present"):
    val nextState = mock[State]
    state.getSurface.expects(baseSurface.id).returns(Right(baseSurface))
    state.removeSurface.expects(baseSurface).returns(Right(nextState))

    val result = worldTest.removeSurface(baseSurface.id)

    inside(result):
      case Right(world) =>
        world.snapshot should be(nextState)

  test("removeSurface propagates error if surface is not found"):
    val expectedError = SurfaceNotFound(baseSurface.id)
    state.getSurface.expects(baseSurface.id).returns(Left(expectedError))

    val result = worldTest.removeSurface(baseSurface.id)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

  test("updateSurface removes old surface and creates updated one"):
    val intermediateState = mock[State]
    val finalState = mock[State]
    val command = SaveSurfaceCommand(baseSurface)

    state.getSurface.expects(baseSurface.id).returns(Right(baseSurface))
    state.removeSurface.expects(baseSurface).returns(Right(intermediateState))
    intermediateState.addSurface.expects(baseSurface).returns(Right(finalState))

    val result = worldTest.updateSurface(command)

    inside(result):
      case Right(world) =>
        world.snapshot should be(finalState)

  test("updateSurface propagates the state error if Surface was not found"):
    val command = SaveSurfaceCommand(baseSurface)
    val expectedError = SurfaceNotFound(baseSurface.id)

    state.getSurface.expects(baseSurface.id).returns(Left(expectedError))

    val result = worldTest.updateSurface(command)

    inside(result):
      case Left(error) =>
        error should be(expectedError)