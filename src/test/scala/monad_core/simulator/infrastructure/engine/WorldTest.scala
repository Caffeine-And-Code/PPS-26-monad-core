package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.*
import monad_core.engine.core.events.EngineEvent
import monad_core.engine.core.events.EngineEvent.{EntityCreated, EntityRemoved, EntityUpdated}
import monad_core.engine.model.*
import monad_core.simulator.application.engine.errors.EngineErrorAdapted
import monad_core.simulator.application.engine.world.{
  SaveEntityCommand,
  SaveSurfaceCommand,
  SaveTeamCommand,
  World
}
import monad_core.simulator.infrastructure.engine.MonadCoreWorld
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ListBuffer

class WorldTest extends AnyFunSuite with Matchers with MockFactory with Inside:

  val baseEntity: Entity   = Entity.circle("id", Vector2D(0, 0), 2).value
  val baseSurface: Surface = Surface.circle("id", Vector2D(0, 0), 2).value
  val baseTeam: Team       = Team(TeamId("id").value, Set.empty).value

  def newWorld(
      onEvents: Vector[EngineEvent] => Unit = _ => ()
  ): (Scene, World) =
    val state: Scene = mock[Scene]
    (state, MonadCoreWorld(state, onEvents))

  test("createEntity actually creates the entity"):
    val publishedEvents    = ListBuffer.empty[EngineEvent]
    val (state, worldTest) = newWorld(events => publishedEvents ++= events)
    val nextState: Scene   = mock[Scene]
    state.addEntity.expects(baseEntity).returns(Right(nextState))
    val command = SaveEntityCommand(baseEntity)

    val result = worldTest.createEntity(command)

    result shouldBe Right(())
    worldTest.scene shouldBe nextState
    publishedEvents.toList shouldBe List(EntityCreated(baseEntity))

  test("createEntity carries the state errors"):
    val publishedEvents    = ListBuffer.empty[EngineEvent]
    val (state, worldTest) = newWorld(events => publishedEvents ++= events)
    val expectedError      = CannotAddEntity(CannotAddAlreadyPresentElementInMap(baseEntity.id))

    state.addEntity.expects(baseEntity).returns(Left(expectedError))
    val command = SaveEntityCommand(baseEntity)

    val result = worldTest.createEntity(command)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))
    publishedEvents shouldBe empty

  test("removeEntity removes entity successfully when present"):
    val publishedEvents    = ListBuffer.empty[EngineEvent]
    val (state, worldTest) = newWorld(events => publishedEvents ++= events)
    val nextState          = mock[Scene]
    state.getEntity.expects(baseEntity.id).returns(Right(baseEntity))
    state.removeEntity.expects(baseEntity).returns(Right(nextState))

    val result = worldTest.removeEntity(baseEntity.id.value)

    result shouldBe Right(())
    worldTest.scene shouldBe nextState
    publishedEvents.toList shouldBe List(EntityRemoved(baseEntity))

  test("removeEntity propagates error if entity is not found"):
    val (state, worldTest) = newWorld()
    val expectedError      = EntityNotFound(baseEntity.id)
    state.getEntity.expects(baseEntity.id).returns(Left(expectedError))

    val result = worldTest.removeEntity(baseEntity.id.value)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))

  test("updateEntity atomically replaces the entity and publishes one update event"):
    val publishedEvents    = ListBuffer.empty[EngineEvent]
    val (state, worldTest) = newWorld(events => publishedEvents ++= events)
    val intermediateState  = mock[Scene]
    val finalState         = mock[Scene]
    val updatedEntity      = baseEntity.moveTo(Vector2D(10, 20))
    val command            = SaveEntityCommand(updatedEntity)

    state.getEntity.expects(baseEntity.id).returns(Right(baseEntity))
    state.removeEntity.expects(baseEntity).returns(Right(intermediateState))
    intermediateState.addEntity.expects(updatedEntity).returns(Right(finalState))

    val result = worldTest.updateEntity(command)

    result shouldBe Right(())
    worldTest.scene shouldBe finalState
    publishedEvents.toList shouldBe List(EntityUpdated(baseEntity, updatedEntity))

  test("updateEntity propagates the state error if Entity was not found"):
    val publishedEvents    = ListBuffer.empty[EngineEvent]
    val (state, worldTest) = newWorld(events => publishedEvents ++= events)
    val command            = SaveEntityCommand(baseEntity)
    val expectedError      = EntityNotFound(baseEntity.id)

    state.getEntity.expects(baseEntity.id).returns(Left(expectedError))

    val result = worldTest.updateEntity(command)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))
    publishedEvents shouldBe empty

  test("updateEntity preserves the original scene and emits no event if replacement fails"):
    val publishedEvents    = ListBuffer.empty[EngineEvent]
    val (state, worldTest) = newWorld(events => publishedEvents ++= events)
    val intermediateState  = mock[Scene]
    val updatedEntity      = baseEntity.moveTo(Vector2D(10, 20))
    val command            = SaveEntityCommand(updatedEntity)
    val expectedError      = CannotAddEntity(CannotAddAlreadyPresentElementInMap(baseEntity.id))

    state.getEntity.expects(baseEntity.id).returns(Right(baseEntity))
    state.removeEntity.expects(baseEntity).returns(Right(intermediateState))
    intermediateState.addEntity.expects(updatedEntity).returns(Left(expectedError))

    val result = worldTest.updateEntity(command)

    result shouldBe Left(EngineErrorAdapted(expectedError))
    worldTest.scene shouldBe state
    publishedEvents shouldBe empty

  test("createTeam actually creates the team"):
    val (state, worldTest) = newWorld()
    val nextState: Scene   = mock[Scene]
    state.addTeam.expects(baseTeam).returns(Right(nextState))
    val command = SaveTeamCommand(baseTeam)

    val result = worldTest.createTeam(command)

    result shouldBe Right(())
    worldTest.scene shouldBe nextState

  test("createTeam carries the state errors"):
    val (state, worldTest) = newWorld()
    val expectedError      = CannotAddTeam(CannotAddAlreadyPresentElementInMap(baseTeam.id))
    state.addTeam.expects(baseTeam).returns(Left(expectedError))
    val command = SaveTeamCommand(baseTeam)

    val result = worldTest.createTeam(command)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))

  test("removeTeam removes team successfully when present"):
    val (state, worldTest) = newWorld()
    val nextState          = mock[Scene]
    state.getTeam.expects(baseTeam.id).returns(Right(baseTeam))
    state.removeTeam.expects(baseTeam).returns(Right(nextState))

    val result = worldTest.removeTeam(baseTeam.id.value)

    result shouldBe Right(())
    worldTest.scene shouldBe nextState

  test("removeTeam propagates error if team is not found"):
    val (state, worldTest) = newWorld()
    val expectedError      = TeamNotFound(baseTeam.id)
    state.getTeam.expects(baseTeam.id).returns(Left(expectedError))

    val result = worldTest.removeTeam(baseTeam.id.value)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))

  test("updateTeam removes old team and creates updated one"):
    val (state, worldTest) = newWorld()
    val intermediateState  = mock[Scene]
    val finalState         = mock[Scene]
    val command            = SaveTeamCommand(baseTeam)

    state.getTeam.expects(baseTeam.id).returns(Right(baseTeam))
    state.removeTeam.expects(baseTeam).returns(Right(intermediateState))
    intermediateState.addTeam.expects(baseTeam).returns(Right(finalState))

    val result = worldTest.updateTeam(command)

    result shouldBe Right(())
    worldTest.scene shouldBe finalState

  test("updateTeam propagates the state error if Team was not found"):
    val (state, worldTest) = newWorld()
    val command            = SaveTeamCommand(baseTeam)
    val expectedError      = TeamNotFound(baseTeam.id)

    state.getTeam.expects(baseTeam.id).returns(Left(expectedError))

    val result = worldTest.updateTeam(command)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))

  test("createSurface actually creates the surface"):
    val (state, worldTest) = newWorld()
    val nextState: Scene   = mock[Scene]

    state.addSurface.expects(baseSurface).returns(Right(nextState))
    val command = SaveSurfaceCommand(baseSurface)

    val result = worldTest.createSurface(command)

    result shouldBe Right(())
    worldTest.scene shouldBe nextState

  test("createSurface carries the state errors"):
    val (state, worldTest) = newWorld()
    val expectedError      = CannotAddSurface(CannotAddAlreadyPresentElementInMap(baseSurface.id))
    state.addSurface.expects(baseSurface).returns(Left(expectedError))
    val command = SaveSurfaceCommand(baseSurface)

    val result = worldTest.createSurface(command)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))

  test("removeSurface removes surface successfully when present"):
    val (state, worldTest) = newWorld()
    val nextState          = mock[Scene]
    state.getSurface.expects(baseSurface.id).returns(Right(baseSurface))
    state.removeSurface.expects(baseSurface).returns(Right(nextState))

    val result = worldTest.removeSurface(baseSurface.id.value)

    result shouldBe Right(())
    worldTest.scene shouldBe nextState

  test("removeSurface propagates error if surface is not found"):
    val (state, worldTest) = newWorld()
    val expectedError      = SurfaceNotFound(baseSurface.id)
    state.getSurface.expects(baseSurface.id).returns(Left(expectedError))

    val result = worldTest.removeSurface(baseSurface.id.value)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))

  test("updateSurface removes old surface and creates updated one"):
    val (state, worldTest) = newWorld()
    val intermediateState  = mock[Scene]
    val finalState         = mock[Scene]
    val command            = SaveSurfaceCommand(baseSurface)

    state.getSurface.expects(baseSurface.id).returns(Right(baseSurface))
    state.removeSurface.expects(baseSurface).returns(Right(intermediateState))
    intermediateState.addSurface.expects(baseSurface).returns(Right(finalState))

    val result = worldTest.updateSurface(command)

    result shouldBe Right(())
    worldTest.scene shouldBe finalState

  test("updateSurface propagates the state error if Surface was not found"):
    val (state, worldTest) = newWorld()
    val command            = SaveSurfaceCommand(baseSurface)
    val expectedError      = SurfaceNotFound(baseSurface.id)

    state.getSurface.expects(baseSurface.id).returns(Left(expectedError))

    val result = worldTest.updateSurface(command)

    inside(result):
      case Left(error) =>
        error should be(EngineErrorAdapted(expectedError))

  test("resize actually changes the world size"):
    val worldTest = MonadCoreWorld(Scene())

    val result = worldTest.resize(800.0, 600.0)

    result shouldBe Right(())
    worldTest.scene.bounds.upperLeft shouldBe Vector2D(0.0, 0.0)
    worldTest.scene.bounds.lowerRight shouldBe Vector2D(800.0, 600.0)

  test("resize propagates the state error"):
    val worldTest = MonadCoreWorld(Scene())

    val result = worldTest.resize(0.0, 600.0)

    result shouldBe Left(
      EngineErrorAdapted(WorldBoundsCannotBeNegativeOrZero())
    )
