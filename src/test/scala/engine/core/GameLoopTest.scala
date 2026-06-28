package engine.core

import engine.core.traits.{Physics, RenderEngine, Scene}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalamock.scalatest.MockFactory

class GameLoopTest extends AnyFunSuite with Matchers with MockFactory :

  val DefaultTickTime = 16_000_000L
  val DefaultMaxFrameTime = 250_000_000L
  val MockScene: Scene = mock[Scene]
  val MockPhysics: Physics = mock[Physics]
  val MockRender: RenderEngine = mock[RenderEngine]
  val InitialTime = 0L

  test("a game loop should start in edit mode by default"):
    val gameLoop = GameLoop()

    gameLoop.mode shouldBe EditMode

  test("a game loop should be able to switch between edit mode and simulation mode"):
    val gameLoop = GameLoop()

    val simulationLoop = gameLoop.withMode(SimulationMode)

    simulationLoop.mode shouldBe SimulationMode

  test("a game loop should be able to switch between simulation mode and edit mode"):
    val gameLoop = GameLoop()

    val simulationLoop = gameLoop.withMode(SimulationMode)
    val editLoop = simulationLoop.withMode(EditMode)

    editLoop.mode shouldBe EditMode

  test("our game loop should have a default tick period of 60Hz"):
    val gameLoop = GameLoop()

    gameLoop.tickTime shouldBe DefaultTickTime

  test("a game loop should allow configuring a custom tick time"):
    val gameLoop = GameLoop()
    val differentTickTime = 10_000_000L

    val differentTickLoop = gameLoop.withTickTime(differentTickTime)

    differentTickLoop.tickTime shouldBe differentTickTime

  test("a game loop should not be running by default"):
    val gameLoop = GameLoop()

    gameLoop.isRunning shouldBe false

  test("a game loop can be started and stopped"):
    val gameLoop = GameLoop()
    val startedLoop = gameLoop.start()
    val stoppedLoop = startedLoop.stop()

    startedLoop.isRunning shouldBe true
    stoppedLoop.isRunning shouldBe false

  test("if the game loop is not running, it should not update the physics"):
    val initialLoop = GameLoop()
    val currentTime = 1_000_000L

    MockPhysics.step.expects(*, *).never()
    MockRender.render.expects(*, *).anyNumberOfTimes()

    val (currentScene, _) = initialLoop.tick(MockScene, MockPhysics, MockRender, currentTime)

    currentScene shouldBe MockScene

  test("if the game loop is in edit mode, it should not update the physics"):
    val initialLoop = GameLoop().start()
    val currentTime = 1_000_000L

    MockPhysics.step.expects(*, *).never()
    MockRender.render.expects(*, *).anyNumberOfTimes()

    val (currentScene, _) = initialLoop.tick(MockScene, MockPhysics, MockRender, currentTime)

    currentScene shouldBe MockScene

  test("if the game loop is in edit mode or not running, it should still update its last timestamp"):
    val currentTime = 30_000_000L
    val initialLoop = GameLoop(lastTime = InitialTime)

    MockRender.render.expects(*, *).anyNumberOfTimes()

    val (_, currentLoop) = initialLoop.tick(MockScene, MockPhysics, MockRender, currentTime)

    currentLoop.lastTime shouldBe currentTime

  test("in simulation mode, passing less than one tick period should not invoke the physics engine"):
    val timeDifference = 1L
    val currentTime = DefaultTickTime - timeDifference
    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime)

    MockPhysics.step.expects(*, *).never()
    MockRender.render.expects(*, *).anyNumberOfTimes()

    val (currentScene, currentLoop) = initialLoop.tick(MockScene, MockPhysics, MockRender, currentTime)

    currentScene shouldBe MockScene
    currentLoop.lastTime shouldBe currentTime

  test("in simulation mode, passing exactly one tick period should invoke the physics engine once"):
    val updatedScene = mock[Scene]
    val currentTime = DefaultTickTime
    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime)

    MockPhysics.step.expects(MockScene, currentTime).returning(updatedScene).once()
    MockRender.render.expects(*, *).anyNumberOfTimes()

    val (currentScene, currentLoop) = initialLoop.tick(MockScene, MockPhysics, MockRender, currentTime)

    currentScene shouldBe updatedScene
    currentLoop.lastTime shouldBe currentTime

  test("in simulation mode, passing two tick periods should invoke the physics engine twice"):
    val sceneStep1 = mock[Scene]
    val sceneStep2 = mock[Scene]
    val currentTime = DefaultTickTime * 2
    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime)

    MockRender.render.expects(*, *).anyNumberOfTimes()
    inSequence :
      MockPhysics.step.expects(MockScene, DefaultTickTime).returning(sceneStep1).once()
      MockPhysics.step.expects(sceneStep1, DefaultTickTime).returning(sceneStep2).once()

    val (currentScene, currentLoop) = initialLoop.tick(MockScene, MockPhysics, MockRender, currentTime)

    currentScene shouldBe sceneStep2
    currentLoop.lastTime shouldBe currentTime

  test("in simulation mode, remaining time after fixed updates must be saved in the accumulator"):
    val updatedScene = mock[Scene]
    val currentTime = 20_000_000L
    val correctAccumulator = 4_000_000L
    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime)

    MockPhysics.step.expects(MockScene, DefaultTickTime).returning(updatedScene).once()
    MockRender.render.expects(*, *).anyNumberOfTimes()

    val (_, currentLoop) = initialLoop.tick(MockScene, MockPhysics, MockRender, currentTime)

    currentLoop.accumulator shouldBe correctAccumulator

  test("massive lag spikes must be clamped to prevent overload"):
    val currentTime = 1_000_000_000L
    val correctAccumulator = 10_000_000L
    val correctIterationNumber = 15 // #iterations = 250ms / 16ms = 15
    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime, maxFrameTime = DefaultMaxFrameTime)

    MockPhysics.step.expects(*, *).repeated(correctIterationNumber)
    MockRender.render.expects(*, *).anyNumberOfTimes()

    val (_, currentLoop) = initialLoop.tick(MockScene, MockPhysics, MockRender, currentTime)

    currentLoop.accumulator shouldBe correctAccumulator

  test("game loop must invoke the render engine passing the correct interpolation alpha"):
    val updatedScene = mock[Scene]
    val currentTime = 20_000_000L
    val correctAlpha = 0.25 // alpha = 4ms / 16ms = 0.25
    val correctAccumulator = 4_000_000L
    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime)

    MockPhysics.step.expects(MockScene, DefaultTickTime).returning(updatedScene).once()
    MockRender.render.expects(updatedScene, correctAlpha).once()

    val (_, currentLoop) = initialLoop.tick(MockScene, MockPhysics, MockRender, currentTime)

    currentLoop.accumulator shouldBe correctAccumulator

  test("game loop must invoke the render engine with static alpha when stopped"):
    val currentTime = 20_000_000L
    val correctAlpha = 1.0
    val initialLoop = GameLoop(lastTime = InitialTime)

    MockRender.render.expects(MockScene, correctAlpha).once()

    val (currentScene, currentLoop) = initialLoop.tick(MockScene, MockPhysics, MockRender, currentTime)

    currentScene shouldBe MockScene

  test("stopping or switching mode must freeze the simulation, which can then be resumed"):
    val updatedScene = mock[Scene]
    val partialTime1 = 16_000_000L
    val partialTime2 = 32_000_000L
    val partialTime3 = 48_000_000L
    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = InitialTime)

    MockRender.render.expects(*, *).anyNumberOfTimes()
    inSequence :
      MockPhysics.step.expects(MockScene, DefaultTickTime).returning(updatedScene).once()
      MockPhysics.step.expects(*, DefaultTickTime).returning(updatedScene).once()

    val (scene1, loop1) = initialLoop.tick(MockScene, MockPhysics, MockRender, partialTime1)
    val loopPaused = loop1.stop()
    val (scene2, loop2) = loopPaused.tick(scene1, MockPhysics, MockRender, partialTime2)
    val loopResumed = loop2.start()
    val (scene3, currentLoop) = loopResumed.tick(scene2, MockPhysics, MockRender, partialTime3)

    scene2 shouldBe scene1
    scene3 shouldBe updatedScene
    currentLoop.isRunning shouldBe true