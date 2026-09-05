package monad_core.engine.core

import helpers.mocks.MockStateHelper
import monad_core.engine.core.LoopMode.{EditMode, SimulationMode}
import monad_core.engine.core.events.EngineEvent
import monad_core.engine.core.traits.{PhysicsEngine, PhysicsStep, State}
import monad_core.engine.core.{
  GameLoop,
  InvalidAccumulator,
  InvalidLastTime,
  InvalidMaxFrameTime,
  InvalidMaxFrameTimeTickTimeRatio,
  InvalidTickTime,
  LoopMode
}
import monad_core.engine.simulator.Painter
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GameLoopTest extends AnyFunSuite with Matchers with MockFactory with MockStateHelper:

  val DefaultTickTime        = 16_000_000L
  val DefaultMaxFrameTime    = 250_000_000L
  val InitialTime            = 0L
  val StandardLoop: GameLoop = GameLoop().value

  given MockPhysics: PhysicsEngine = mock[PhysicsEngine]

  test("a game loop should start in edit mode by default"):
    StandardLoop.mode shouldBe LoopMode.EditMode

  test("a game loop should be able to switch between edit mode and simulation mode"):
    val simulationLoop = StandardLoop.withMode(SimulationMode)

    simulationLoop.mode shouldBe SimulationMode

  test("a game loop should be able to switch between simulation mode and edit mode"):
    val simulationLoop = StandardLoop.withMode(SimulationMode)
    val editLoop       = simulationLoop.withMode(EditMode)

    editLoop.mode shouldBe EditMode

  test("our game loop should have a default tick period of 60Hz"):
    StandardLoop.tickTime shouldBe DefaultTickTime

  test("our game loop should have a default max frame time"):
    StandardLoop.maxFrameTime shouldBe DefaultMaxFrameTime

  test("a game loop should allow configuring a custom tick time"):
    val differentTickTime = 10_000_000L

    val differentTickLoop = StandardLoop.withTickTime(differentTickTime).value

    differentTickLoop.tickTime shouldBe differentTickTime

  test(
    "a game loop should reject non-positive values or values exceeding max frame time for tick time"
  ):
    StandardLoop.withTickTime(0L) shouldBe Left(InvalidTickTime(0L))
    StandardLoop.withTickTime(DefaultMaxFrameTime + 1L) shouldBe Left(
      InvalidMaxFrameTimeTickTimeRatio(DefaultMaxFrameTime, DefaultMaxFrameTime + 1L)
    )

  test("a game loop should not be running by default"):
    StandardLoop.isRunning shouldBe false

  test("a game loop can be started and stopped"):
    val startedLoop = StandardLoop.start()
    val stoppedLoop = startedLoop.stop()

    startedLoop.isRunning shouldBe true
    stoppedLoop.isRunning shouldBe false

  test("if the game loop is not running, it should not update the physics"):
    given painter: Painter = mock[Painter]

    val scene       = stateWithEntities(List.empty)
    val currentTime = 1_000_000L

    MockPhysics.step.expects(*, *).never()

    val result = StandardLoop.tick(scene, currentTime).value

    result.state shouldBe scene
    result.previousState shouldBe scene

  test("the game loop should have have two modes: edit and simulation"):
    LoopMode.values should contain allOf (LoopMode.EditMode, LoopMode.SimulationMode)
    LoopMode.valueOf("EditMode") shouldBe LoopMode.EditMode
    LoopMode.fromOrdinal(1) shouldBe LoopMode.SimulationMode

  test("if the game loop is in edit mode, it should not update the physics"):
    val scene       = stateWithEntities(List.empty)
    val initialLoop = StandardLoop.start()
    val currentTime = 1_000_000L

    MockPhysics.step.expects(*, *).never()

    val result = initialLoop.tick(scene, currentTime).value

    result.state shouldBe scene

  test(
    "if the game loop is in edit mode or not running, it should still update its last timestamp"
  ):
    val scene       = stateWithEntities(List.empty)
    val currentTime = 30_000_000L
    val initialLoop = GameLoop(lastTime = InitialTime).value

    val result = initialLoop.tick(scene, currentTime).value

    val secondResult = initialLoop.tick(scene, currentTime).value

    result.loop.lastTime shouldBe currentTime

  test(
    "in simulation mode, passing less than one tick period should not invoke the physics engine"
  ):
    val scene          = stateWithEntities(List.empty)
    val timeDifference = 1L
    val currentTime    = DefaultTickTime - timeDifference
    val initialLoop =
      GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime).value

    MockPhysics.step.expects(*, *).never()

    val result = initialLoop.tick(scene, currentTime).value

    result.state shouldBe scene
    result.previousState shouldBe scene
    result.loop.lastTime shouldBe currentTime

  test("in simulation mode, passing exactly one tick period should invoke the physics engine once"):
    val scene        = stateWithEntities(List.empty)
    val updatedScene = stateWithEntities(List.empty)
    val currentTime  = DefaultTickTime
    val initialLoop =
      GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime).value

    MockPhysics.step
      .expects(scene, currentTime)
      .returning(Right(PhysicsStep(updatedScene)))
      .once()

    val result = initialLoop.tick(scene, currentTime).value

    result.state shouldBe updatedScene
    result.previousState shouldBe scene
    result.loop.lastTime shouldBe currentTime

  test("in simulation mode, passing two tick periods should invoke the physics engine twice"):
    val scene       = stateWithEntities(List.empty)
    val sceneStep1  = stateWithEntities(List.empty)
    val sceneStep2  = stateWithEntities(List.empty)
    val currentTime = DefaultTickTime * 2
    val initialLoop =
      GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime).value

    inSequence:
      MockPhysics.step
        .expects(scene, DefaultTickTime)
        .returning(Right(PhysicsStep(sceneStep1)))
        .once()
      MockPhysics.step
        .expects(sceneStep1, DefaultTickTime)
        .returning(Right(PhysicsStep(sceneStep2)))
        .once()

    val result = initialLoop.tick(scene, currentTime).value

    result.state shouldBe sceneStep2
    result.previousState shouldBe sceneStep1
    result.loop.lastTime shouldBe currentTime

  test("in simulation mode, remaining time after fixed updates must be saved in the accumulator"):
    val scene              = stateWithEntities(List.empty)
    val updatedScene       = stateWithEntities(List.empty)
    val currentTime        = 20_000_000L
    val correctAccumulator = 4_000_000L
    val initialLoop =
      GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime).value

    MockPhysics.step
      .expects(scene, DefaultTickTime)
      .returning(Right(PhysicsStep(updatedScene)))
      .once()

    val result = initialLoop.tick(scene, currentTime).value

    result.loop.accumulator shouldBe correctAccumulator

  test("massive lag spikes must be clamped to prevent overload"):
    val scene                  = stateWithEntities(List.empty)
    val currentTime            = 1_000_000_000L
    val correctAccumulator     = 10_000_000L
    val correctIterationNumber = 15 // #iterations = 250ms / 16ms = 15
    val initialLoop = GameLoop(
      mode = SimulationMode,
      isRunning = true,
      lastTime = InitialTime,
      maxFrameTime = DefaultMaxFrameTime
    ).value

    MockPhysics.step
      .expects(*, *)
      .returning(Right(PhysicsStep(scene)))
      .repeated(correctIterationNumber)

    val result = initialLoop.tick(scene, currentTime).value

    result.loop.accumulator shouldBe correctAccumulator

  test("stopping or switching mode must freeze the simulation, which can then be resumed"):
    val scene        = stateWithEntities(List.empty)
    val updatedScene = stateWithEntities(List.empty)
    val partialTime1 = 16_000_000L
    val partialTime2 = 32_000_000L
    val partialTime3 = 48_000_000L
    val initialLoop =
      GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime).value

    MockPhysics.step
      .expects(scene, DefaultTickTime)
      .returning(Right(PhysicsStep(updatedScene)))
      .once()
    MockPhysics.step
      .expects(*, DefaultTickTime)
      .returning(Right(PhysicsStep(updatedScene)))
      .once()

    val result1     = initialLoop.tick(scene, partialTime1).value
    val loopPaused  = result1.loop.stop()
    val result2     = loopPaused.tick(result1.state, partialTime2).value
    val loopResumed = result2.loop.start()
    val result3     = loopResumed.tick(result2.state, partialTime3).value

    result2.state shouldBe result1.state
    result3.state shouldBe updatedScene
    result3.loop.isRunning shouldBe true

  test("events from every fixed update should be returned in chronological order"):
    val scene       = stateWithEntities(List.empty)
    val firstEvent  = mock[EngineEvent]
    val secondEvent = mock[EngineEvent]
    val sceneStep1  = stateWithEntities(List.empty)
    val sceneStep2  = stateWithEntities(List.empty)
    val currentTime = DefaultTickTime * 2
    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true).value

    inSequence:
      MockPhysics.step
        .expects(scene, DefaultTickTime)
        .returning(Right(PhysicsStep(sceneStep1, Vector(firstEvent))))
        .once()
      MockPhysics.step
        .expects(sceneStep1, DefaultTickTime)
        .returning(Right(PhysicsStep(sceneStep2, Vector(secondEvent))))
        .once()

    val result = initialLoop.tick(scene, currentTime).value

    result.events shouldBe Vector(firstEvent, secondEvent)

  test("a game loop should be InvalidTickTime when tick time is non-positive"):
    val invalidTickTime = 0L
    GameLoop(tickTime = invalidTickTime) shouldBe Left(InvalidTickTime(invalidTickTime))

  test("a game loop should be InvalidLastTime when last time value is negative"):
    val invalidLastTime = -1L
    GameLoop(lastTime = invalidLastTime) shouldBe Left(InvalidLastTime(invalidLastTime))

  test("a game loop should allow last time value to be zero"):
    val validLastTime = 0L
    val loop          = GameLoop(lastTime = validLastTime)
    loop.isRight shouldBe true

  test("a game loop should allow last time value to be positive"):
    val validLastTime = 1L
    val loop          = GameLoop(lastTime = validLastTime)
    loop.isRight shouldBe true

  test("a game loop should be InvalidAccumulator when accumulator is negative"):
    val invalidAccumulator = -1L
    GameLoop(accumulator = invalidAccumulator) shouldBe Left(InvalidAccumulator(invalidAccumulator))

  test("a game loop should be InvalidMaxFrameTime when max frame time is non-positive"):
    val invalidTickTime = 0L
    GameLoop(maxFrameTime = invalidTickTime) shouldBe Left(InvalidMaxFrameTime(invalidTickTime))

  test(
    "GameLoop should be InvalidMaxFrameTimeTickTimeRatio when max frame time is less than tick time"
  ):
    val invalidMaxFrameTime = DefaultTickTime - 1L
    GameLoop(tickTime = DefaultTickTime, maxFrameTime = invalidMaxFrameTime) shouldBe Left(
      InvalidMaxFrameTimeTickTimeRatio(invalidMaxFrameTime, DefaultTickTime)
    )

  test("GameLoop should allow max frame time to equal tick time"):
    val loop = GameLoop(
      tickTime = DefaultTickTime,
      maxFrameTime = DefaultTickTime
    )

    loop.isRight shouldBe true

  test("if a running game loop is in edit mode, it should not update the physics"):
    val scene       = stateWithEntities(List.empty)
    val currentTime = DefaultTickTime
    val editLoop    = GameLoop(mode = EditMode, isRunning = true)

    MockPhysics.step.expects(*, *).never()

    val result = editLoop.value.tick(scene, currentTime).value

    result.state shouldBe scene
    result.loop.lastTime shouldBe currentTime

  test("default game loop should be valid and have default values"):
    val defaultLoop = GameLoop.default()

    defaultLoop.mode shouldBe EditMode
    defaultLoop.tickTime shouldBe DefaultTickTime
    defaultLoop.isRunning shouldBe false
    defaultLoop.lastTime shouldBe InitialTime
    defaultLoop.accumulator shouldBe 0L
    defaultLoop.maxFrameTime shouldBe DefaultMaxFrameTime

  test("withTickTime should reject a negative tick time"):
    StandardLoop.withTickTime(-1L) shouldBe Left(InvalidTickTime(-1L))

  test("withTickTime should allow a tick time equal to max frame time"):
    StandardLoop.withTickTime(DefaultMaxFrameTime).value.tickTime shouldBe DefaultMaxFrameTime

  test("GameLoop should allow a positive initial accumulator"):
    GameLoop(accumulator = 1L).value.accumulator shouldBe 1L
