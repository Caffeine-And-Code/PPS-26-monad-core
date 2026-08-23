package integrations.monad_core.engine.simulator

import monad_core.engine.core.events.EngineEvent
import monad_core.engine.core.events.EngineEvent.EntityCreated
import monad_core.engine.core.traits.State
import monad_core.engine.core.{
  InvalidMaxFrameTime,
  InvalidMaxFrameTimeTickTimeRatio,
  InvalidTickTime,
  LoopMode
}
import monad_core.engine.geometry.ShapeCollision.shapeCollidesWithShape
import monad_core.engine.geometry.ShapeContainment.shapeContainsPoint
import monad_core.engine.helper.PhysicsRuleHelper.makeDummyRule
import monad_core.engine.model.{Entity, Scene, Vector2D}
import monad_core.engine.physics.core.{PhysicsError, PhysicsManager, PhysicsRuleError}
import monad_core.engine.simulator.EngineFacade
import monad_core.engine.simulator.EngineFacade.DefaultTickTime
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EngineFacadeTest extends AnyFunSuite with Matchers:

  private def physicsReturning(
      action: (State, Long) => Either[PhysicsError, State],
      events: Vector[EngineEvent] = Vector.empty
  ): PhysicsManager =
    PhysicsManager(Vector(makeDummyRule(action = action, events = events)))

  test("the default session is stopped"):
    val session = EngineFacade.default

    val result = EngineFacade.isRunning(session)

    result shouldBe false

  test("the default session is in edit mode"):
    val session = EngineFacade.default

    val result = EngineFacade.mode(session)

    result shouldBe LoopMode.EditMode

  test("a session can be created with a valid custom configuration"):
    val tickTime     = 10L
    val maxFrameTime = 20L

    val result = EngineFacade.create(tickTime, maxFrameTime)

    result.isRight shouldBe true

  test("session creation rejects a non-positive tick time"):
    val invalidTickTime = 0L

    val result = EngineFacade.create(tickTime = invalidTickTime)

    result shouldBe Left(InvalidTickTime(invalidTickTime))

  test("session creation rejects a non-positive maximum frame time"):
    val invalidMaxFrameTime = 0L

    val result = EngineFacade.create(maxFrameTime = invalidMaxFrameTime)

    result shouldBe Left(InvalidMaxFrameTime(invalidMaxFrameTime))

  test("session creation rejects a maximum frame time lower than the tick time"):
    val tickTime     = 20L
    val maxFrameTime = 10L

    val result = EngineFacade.create(tickTime, maxFrameTime)

    result shouldBe Left(InvalidMaxFrameTimeTickTimeRatio(maxFrameTime, tickTime))

  test("starting a session marks the returned session as running"):
    val initialSession = EngineFacade.default

    val runningSession = EngineFacade.start(initialSession)

    EngineFacade.isRunning(runningSession) shouldBe true

  test("starting a session switches the returned session to simulation mode"):
    val initialSession = EngineFacade.default

    val runningSession = EngineFacade.start(initialSession)

    EngineFacade.mode(runningSession) shouldBe LoopMode.SimulationMode

  test("starting a session does not mutate the original session"):
    val initialSession = EngineFacade.default

    EngineFacade.start(initialSession)

    EngineFacade.isRunning(initialSession) shouldBe false

  test("stopping a session marks the returned session as stopped"):
    val runningSession = EngineFacade.start(EngineFacade.default)

    val stoppedSession = EngineFacade.stop(runningSession)

    EngineFacade.isRunning(stoppedSession) shouldBe false

  test("stopping a session switches the returned session to edit mode"):
    val runningSession = EngineFacade.start(EngineFacade.default)

    val stoppedSession = EngineFacade.stop(runningSession)

    EngineFacade.mode(stoppedSession) shouldBe LoopMode.EditMode

  test("stopping a session does not mutate the running session"):
    val runningSession = EngineFacade.start(EngineFacade.default)

    EngineFacade.stop(runningSession)

    EngineFacade.isRunning(runningSession) shouldBe true

  test("ticking a stopped session does not evaluate physics"):
    var physicsEvaluated = false
    val physics = physicsReturning { (_, _) =>
      physicsEvaluated = true
      Right(Scene())
    }
    val session = EngineFacade.default

    EngineFacade.tick(session, Scene(), DefaultTickTime, physics)

    physicsEvaluated shouldBe false

  test("tick uses the default physics manager when none is supplied"):
    val initialScene = Scene()
    val session      = EngineFacade.start(EngineFacade.default)

    val result = EngineFacade.tick(session, initialScene, DefaultTickTime)

    result.isRight shouldBe true

  test("a successful tick returns the state produced by physics"):
    val initialScene = Scene()
    val entity       = Entity.circle("entity", Vector2D(0, 0), 1).value
    val updatedScene = initialScene.addEntity(entity).value
    val physics      = physicsReturning((_, _) => Right(updatedScene))
    val session      = EngineFacade.start(EngineFacade.default)

    val result = EngineFacade.tick(session, initialScene, DefaultTickTime, physics).value

    result.state shouldBe updatedScene

  test("a successful tick exposes the state preceding the latest fixed update"):
    val initialScene = Scene()
    val firstScene = initialScene
      .addEntity(Entity.circle("first", Vector2D(0, 0), 1).value)
      .value
    val secondScene = firstScene.addEntity(Entity.circle("second", Vector2D(2, 0), 1).value).value
    val physics = physicsReturning { (state, _) =>
      if state == initialScene then Right(firstScene) else Right(secondScene)
    }
    val session = EngineFacade.start(EngineFacade.default)

    val result = EngineFacade
      .tick(session, initialScene, DefaultTickTime * 2, physics)
      .value

    result.previousState shouldBe firstScene
    result.state shouldBe secondScene

  test("a successful tick returns the events produced by physics"):
    val initialScene = Scene()
    val entity       = Entity.circle("entity", Vector2D(0, 0), 1).value
    val event        = EntityCreated(entity)
    val physics      = physicsReturning((_, _) => Right(initialScene), Vector(event))
    val session      = EngineFacade.start(EngineFacade.default)

    val result = EngineFacade.tick(session, initialScene, DefaultTickTime, physics).value

    result.events shouldBe Vector(event)

  test("a successful tick returns the evolved running session"):
    val initialScene = Scene()
    val physics      = physicsReturning((_, _) => Right(initialScene))
    val session      = EngineFacade.start(EngineFacade.default)

    val result = EngineFacade.tick(session, initialScene, DefaultTickTime, physics).value

    EngineFacade.isRunning(result.nextSession) shouldBe true

  test("an exact fixed update returns a zero interpolation alpha"):
    val initialScene = Scene()
    val physics      = physicsReturning((_, _) => Right(initialScene))
    val session      = EngineFacade.start(EngineFacade.default)

    val result = EngineFacade.tick(session, initialScene, DefaultTickTime, physics).value

    result.alpha shouldBe 0.0

  test("a failed physics update is propagated by tick"):
    val initialScene  = Scene()
    val expectedError = PhysicsRuleError("expected failure")
    val physics       = physicsReturning((_, _) => Left(expectedError))
    val session       = EngineFacade.start(EngineFacade.default)

    val result = EngineFacade.tick(session, initialScene, DefaultTickTime, physics)

    result shouldBe Left(expectedError)
