package monad_core.engine.core

import monad_core.engine.core.LoopMode.{EditMode, SimulationMode}
import monad_core.engine.core.traits.RenderEngine
import monad_core.engine.core.{GameLoop, InvalidAccumulator, InvalidLastTime, InvalidMaxFrameTime, InvalidMaxFrameTimeTickTimeRatio, InvalidTickTime, LoopMode}
import monad_core.engine.physics.core.Physics
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GameLoopTest extends AnyFunSuite with Matchers with MockFactory :

  trait TestScene

  val DefaultTickTime = 16_000_000L
  val DefaultMaxFrameTime = 250_000_000L
  val MockScene: TestScene = mock[TestScene]
  given MockPhysics: Physics[TestScene] = mock[Physics[TestScene]]
  given MockRender: RenderEngine[TestScene] = mock[RenderEngine[TestScene]]
  val InitialTime = 0L

  test("a game loop should start in edit mode by default"):
    val gameLoop = GameLoop().toOption.get

    gameLoop.mode shouldBe LoopMode.EditMode

  test("a game loop should be able to switch between edit mode and simulation mode"):
    val gameLoop = GameLoop().toOption.get

    val simulationLoop = gameLoop.withMode(SimulationMode)

    simulationLoop.mode shouldBe SimulationMode

  test("a game loop should be able to switch between simulation mode and edit mode"):
    val gameLoop = GameLoop().toOption.get

    val simulationLoop = gameLoop.withMode(SimulationMode)
    val editLoop = simulationLoop.withMode(EditMode)

    editLoop.mode shouldBe EditMode

  test("our game loop should have a default tick period of 60Hz"):
    val gameLoop = GameLoop().toOption.get

    gameLoop.tickTime shouldBe DefaultTickTime

  test("our game loop should have a default max frame time"):
    val gameLoop = GameLoop().toOption.get
    gameLoop.maxFrameTime shouldBe DefaultMaxFrameTime

  test("a game loop should allow configuring a custom tick time"):
    val gameLoop = GameLoop().toOption.get
    val differentTickTime = 10_000_000L

    val differentTickLoop = gameLoop.withTickTime(differentTickTime).toOption.get

    differentTickLoop.tickTime shouldBe differentTickTime

  test("a game loop should reject non-positive values or values exceeding max frame time for tick time"):
    val gameLoop = GameLoop().toOption.get

    gameLoop.withTickTime(0L) shouldBe Left(InvalidTickTime(0L))
    gameLoop.withTickTime(DefaultMaxFrameTime + 1L) shouldBe Left(InvalidMaxFrameTimeTickTimeRatio(DefaultMaxFrameTime, DefaultMaxFrameTime + 1L))

  test("a game loop should not be running by default"):
    val gameLoop = GameLoop().toOption.get

    gameLoop.isRunning shouldBe false

  test("a game loop can be started and stopped"):
    val gameLoop = GameLoop().toOption.get
    val startedLoop = gameLoop.start()
    val stoppedLoop = startedLoop.stop()

    startedLoop.isRunning shouldBe true
    stoppedLoop.isRunning shouldBe false

  test("if the game loop is not running, it should not update the physics"):
    val initialLoop = GameLoop().toOption.get
    val currentTime = 1_000_000L

    MockPhysics.step.expects(*, *).never()
    MockRender.render.expects(*, *).anyNumberOfTimes()

    val (currentScene, _) = initialLoop.tick(MockScene, currentTime).getOrElse(fail())

    currentScene shouldBe MockScene

  test("the game loop should have have two modes: edit and simulation"):
    LoopMode.values should contain allOf(LoopMode.EditMode, LoopMode.SimulationMode)
    LoopMode.valueOf("EditMode") shouldBe LoopMode.EditMode
    LoopMode.fromOrdinal(1) shouldBe LoopMode.SimulationMode

  test("if the game loop is in edit mode, it should not update the physics"):
    val initialLoop = GameLoop().toOption.get.start()
    val currentTime = 1_000_000L

    MockPhysics.step.expects(*, *).never()
    MockRender.render.expects(*, *).anyNumberOfTimes()

    val (currentScene, _) = initialLoop.tick(MockScene, currentTime).getOrElse(fail())

    currentScene shouldBe MockScene

  test("if the game loop is in edit mode or not running, it should still update its last timestamp"):
    val currentTime = 30_000_000L
    val initialLoop = GameLoop(lastTime = InitialTime).toOption.get

    MockRender.render.expects(*, *).anyNumberOfTimes()

    val (_, currentLoop) = initialLoop.tick(MockScene, currentTime).getOrElse(fail())

    currentLoop.lastTime shouldBe currentTime

  test("in simulation mode, passing less than one tick period should not invoke the physics engine"):
    val timeDifference = 1L
    val currentTime = DefaultTickTime - timeDifference
    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime).toOption.get

    MockPhysics.step.expects(*, *).never()
    MockRender.render.expects(*, *).anyNumberOfTimes()

    val (currentScene, currentLoop) = initialLoop.tick(MockScene, currentTime).getOrElse(fail())

    currentScene shouldBe MockScene
    currentLoop.lastTime shouldBe currentTime

  test("in simulation mode, passing exactly one tick period should invoke the physics engine once"):
    val updatedScene = mock[TestScene]
    val currentTime = DefaultTickTime
    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime).toOption.get

    MockPhysics.step.expects(MockScene, currentTime).returning(Right(updatedScene)).once()
    MockRender.render.expects(*, *).anyNumberOfTimes()

    val (currentScene, currentLoop) = initialLoop.tick(MockScene, currentTime).getOrElse(fail())

    currentScene shouldBe updatedScene
    currentLoop.lastTime shouldBe currentTime

  test("in simulation mode, passing two tick periods should invoke the physics engine twice"):
    val sceneStep1 = mock[TestScene]
    val sceneStep2 = mock[TestScene]
    val currentTime = DefaultTickTime * 2
    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime).toOption.get

    MockRender.render.expects(*, *).anyNumberOfTimes()
    inSequence :
      MockPhysics.step.expects(MockScene, DefaultTickTime).returning(Right(sceneStep1)).once()
      MockPhysics.step.expects(sceneStep1, DefaultTickTime).returning(Right(sceneStep2)).once()

    val (currentScene, currentLoop) = initialLoop.tick(MockScene, currentTime).getOrElse(fail())

    currentScene shouldBe sceneStep2
    currentLoop.lastTime shouldBe currentTime

  test("in simulation mode, remaining time after fixed updates must be saved in the accumulator"):
    val updatedScene = mock[TestScene]
    val currentTime = 20_000_000L
    val correctAccumulator = 4_000_000L
    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime).toOption.get

    MockPhysics.step.expects(MockScene, DefaultTickTime).returning(Right(updatedScene)).once()
    MockRender.render.expects(*, *).anyNumberOfTimes()

    val (_, currentLoop) = initialLoop.tick(MockScene, currentTime).getOrElse(fail())

    currentLoop.accumulator shouldBe correctAccumulator

  test("massive lag spikes must be clamped to prevent overload"):
    val currentTime = 1_000_000_000L
    val correctAccumulator = 10_000_000L
    val correctIterationNumber = 15 // #iterations = 250ms / 16ms = 15
    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime, maxFrameTime = DefaultMaxFrameTime).toOption.get

    MockPhysics.step.expects(*, *).repeated(correctIterationNumber).returning(Right(MockScene))
    MockRender.render.expects(*, *).anyNumberOfTimes()

    val (_, currentLoop) = initialLoop.tick(MockScene, currentTime).getOrElse(fail())

    currentLoop.accumulator shouldBe correctAccumulator

  test("game loop must invoke the render engine passing the correct interpolation alpha"):
    val updatedScene = mock[TestScene]
    val currentTime = 20_000_000L
    val correctAlpha = 0.25 // alpha = 4ms / 16ms = 0.25
    val correctAccumulator = 4_000_000L
    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime).toOption.get

    MockPhysics.step.expects(MockScene, DefaultTickTime).returning(Right(updatedScene)).once()
    MockRender.render.expects(updatedScene, correctAlpha).once()

    val (_, currentLoop) = initialLoop.tick(MockScene, currentTime).getOrElse(fail())

    currentLoop.accumulator shouldBe correctAccumulator

  test("game loop must invoke the render engine with static alpha when stopped"):
    val currentTime = 20_000_000L
    val correctAlpha = 1.0
    val initialLoop = GameLoop(lastTime = InitialTime).toOption.get

    MockRender.render.expects(MockScene, correctAlpha).once()

    val (currentScene, currentLoop) = initialLoop.tick(MockScene, currentTime).getOrElse(fail())

    currentScene shouldBe MockScene

  test("stopping or switching mode must freeze the simulation, which can then be resumed"):
    val updatedScene = mock[TestScene]
    val partialTime1 = 16_000_000L
    val partialTime2 = 32_000_000L
    val partialTime3 = 48_000_000L
    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime).toOption.get

    MockRender.render.expects(*, *).anyNumberOfTimes()
    inSequence :
      MockPhysics.step.expects(MockScene, DefaultTickTime).returning(Right(updatedScene)).once()
      MockPhysics.step.expects(*, DefaultTickTime).returning(Right(updatedScene)).once()

    val (scene1, loop1) = initialLoop.tick(MockScene, partialTime1).getOrElse(fail())
    val loopPaused = loop1.stop()
    val (scene2, loop2) = loopPaused.tick(scene1, partialTime2).getOrElse(fail())
    val loopResumed = loop2.start()
    val (scene3, currentLoop) = loopResumed.tick(scene2, partialTime3).getOrElse(fail())

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

  test("GameLoop should be InvalidMaxFrameTimeTickTimeRatio when max frame time is less than tick time"):
    val invalidMaxFrameTime = DefaultTickTime - 1L
    GameLoop(tickTime = DefaultTickTime, maxFrameTime = invalidMaxFrameTime) shouldBe Left(InvalidMaxFrameTimeTickTimeRatio(invalidMaxFrameTime, DefaultTickTime))