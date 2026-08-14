package monad_core.engine.core

import monad_core.engine.core.traits.State
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
import monad_core.engine.core.LoopMode.{EditMode, SimulationMode}
import monad_core.engine.core.traits.RenderEngine
import monad_core.engine.core.traits.PhysicsEngine
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GameLoopTest extends AnyFunSuite with Matchers with MockFactory:

  val DefaultTickTime        = 16_000_000L
  val DefaultMaxFrameTime    = 250_000_000L
  val MockState: State       = mock[State]
  val InitialTime            = 0L
  val StandardLoop: GameLoop = GameLoop().value

  given MockPhysics: PhysicsEngine = mock[PhysicsEngine]
  given MockPainter: Painter       = mock[Painter]

  def setupGenericStateCalls(): Unit =
    setupStateCalls(MockState)

  def setupStateCalls(state: State): Unit =
    (() => state.allSurfaces).expects().returns(List.empty).anyNumberOfTimes()
    (() => state.allTeams).expects().returns(List.empty).anyNumberOfTimes()
    (() => state.allEntities).expects().returns(List.empty).anyNumberOfTimes()

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

    val currentTime = 1_000_000L

    MockPhysics.step.expects(*, *).never()
    setupGenericStateCalls()

    val (currentScene, _) = StandardLoop.tick(MockState, currentTime).value

    currentScene shouldBe MockState

  test("the game loop should have have two modes: edit and simulation"):
    LoopMode.values should contain allOf (LoopMode.EditMode, LoopMode.SimulationMode)
    LoopMode.valueOf("EditMode") shouldBe LoopMode.EditMode
    LoopMode.fromOrdinal(1) shouldBe LoopMode.SimulationMode

  test("if the game loop is in edit mode, it should not update the physics"):

    given Painter = mock[Painter]

    val initialLoop = StandardLoop.start()
    val currentTime = 1_000_000L

    MockPhysics.step.expects(*, *).never()
    setupGenericStateCalls()

    val (currentScene, _) = initialLoop.tick(MockState, currentTime).value

    currentScene shouldBe MockState

  test(
    "if the game loop is in edit mode or not running, it should still update its last timestamp"
  ):

    given Painter = mock[Painter]

    val currentTime = 30_000_000L
    val initialLoop = GameLoop(lastTime = InitialTime).value

    setupGenericStateCalls()

    val (_, currentLoop) = initialLoop.tick(MockState, currentTime).value

    currentLoop.lastTime shouldBe currentTime

  test(
    "in simulation mode, passing less than one tick period should not invoke the physics engine"
  ):

    given Painter = mock[Painter]

    val timeDifference = 1L
    val currentTime    = DefaultTickTime - timeDifference
    val initialLoop =
      GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime).value

    MockPhysics.step.expects(*, *).never()
    setupGenericStateCalls()

    val (currentScene, currentLoop) = initialLoop.tick(MockState, currentTime).value

    currentScene shouldBe MockState
    currentLoop.lastTime shouldBe currentTime

  test("in simulation mode, passing exactly one tick period should invoke the physics engine once"):

    given Painter = mock[Painter]

    val updatedScene = mock[State]
    val currentTime  = DefaultTickTime
    val initialLoop =
      GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime).value

    MockPhysics.step.expects(MockState, currentTime).returning(Right(updatedScene)).once()
    setupStateCalls(updatedScene)

    val (currentScene, currentLoop) = initialLoop.tick(MockState, currentTime).value

    currentScene shouldBe updatedScene
    currentLoop.lastTime shouldBe currentTime

  test("in simulation mode, passing two tick periods should invoke the physics engine twice"):

    given Painter = mock[Painter]

    val sceneStep1  = mock[State]
    val sceneStep2  = mock[State]
    val currentTime = DefaultTickTime * 2
    val initialLoop =
      GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime).value

    setupStateCalls(sceneStep2)
    inSequence:
      MockPhysics.step.expects(MockState, DefaultTickTime).returning(Right(sceneStep1)).once()
      MockPhysics.step.expects(sceneStep1, DefaultTickTime).returning(Right(sceneStep2)).once()

    val (currentScene, currentLoop) = initialLoop.tick(MockState, currentTime).value

    currentScene shouldBe sceneStep2
    currentLoop.lastTime shouldBe currentTime

  test("in simulation mode, remaining time after fixed updates must be saved in the accumulator"):

    given Painter = mock[Painter]

    val updatedScene       = mock[State]
    val currentTime        = 20_000_000L
    val correctAccumulator = 4_000_000L
    val initialLoop =
      GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime).value

    MockPhysics.step.expects(MockState, DefaultTickTime).returning(Right(updatedScene)).once()
    setupStateCalls(updatedScene)

    val (_, currentLoop) = initialLoop.tick(MockState, currentTime).value

    currentLoop.accumulator shouldBe correctAccumulator

  test("massive lag spikes must be clamped to prevent overload"):

    given Painter = mock[Painter]

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
      .returning(Right(MockState))
      .repeated(correctIterationNumber)
    setupGenericStateCalls()

    val (_, currentLoop) = initialLoop.tick(MockState, currentTime).value

    currentLoop.accumulator shouldBe correctAccumulator

  test("stopping or switching mode must freeze the simulation, which can then be resumed"):

    given Painter = mock[Painter]

    val updatedScene = mock[State]
    val partialTime1 = 16_000_000L
    val partialTime2 = 32_000_000L
    val partialTime3 = 48_000_000L
    val initialLoop =
      GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime).value

    setupGenericStateCalls()
    setupStateCalls(updatedScene)
    MockPhysics.step.expects(MockState, DefaultTickTime).returning(Right(updatedScene)).once()
    MockPhysics.step.expects(*, DefaultTickTime).returning(Right(updatedScene)).once()

    val (scene1, loop1)       = initialLoop.tick(MockState, partialTime1).value
    val loopPaused            = loop1.stop()
    val (scene2, loop2)       = loopPaused.tick(scene1, partialTime2).value
    val loopResumed           = loop2.start()
    val (scene3, currentLoop) = loopResumed.tick(scene2, partialTime3).value

    scene2 shouldBe scene1
    scene3 shouldBe updatedScene
    currentLoop.isRunning shouldBe true

  test("a game loop should be InvalidTickTime when tick time is non-positive"):
    val invalidTickTime = 0L
    GameLoop(tickTime = invalidTickTime) shouldBe Left(InvalidTickTime(invalidTickTime))

  test("a game loop should be InvalidLastTime when last time value is negative"):
    val invalidLastTime = -1L
    GameLoop(lastTime = invalidLastTime) shouldBe Left(InvalidLastTime(invalidLastTime))

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
    noException should be thrownBy GameLoop(
      tickTime = DefaultTickTime,
      maxFrameTime = DefaultTickTime
    )

  test("if a running game loop is in edit mode, it should not update the physics"):

    given painter: Painter = mock[Painter]

    val currentTime = DefaultTickTime
    val editLoop    = GameLoop(mode = EditMode, isRunning = true)

    MockPhysics.step.expects(*, *).never()
    setupGenericStateCalls()

    val (currentScene, currentLoop) =
      editLoop.value.tick(MockState, currentTime)(using MockPhysics, painter).value

    currentScene shouldBe MockState
    currentLoop.lastTime shouldBe currentTime

  test("default game loop should be valid and have default values"):
    val defaultLoop = GameLoop.default()

    defaultLoop.mode shouldBe EditMode
    defaultLoop.tickTime shouldBe DefaultTickTime
    defaultLoop.isRunning shouldBe false
    defaultLoop.lastTime shouldBe InitialTime
    defaultLoop.accumulator shouldBe 0L
    defaultLoop.maxFrameTime shouldBe DefaultMaxFrameTime
