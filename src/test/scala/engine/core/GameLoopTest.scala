package engine.core

import engine.core.traits.{PhysicsEngine, RenderEngine, Scene}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalamock.scalatest.MockFactory

class GameLoopTest extends AnyFunSuite with Matchers with MockFactory :

  val defaultTickTime = 16_000_000L
  val differentTickTime = 10_000_000L
  val defaultMaxFrameTime = 250_000_000L
  val mockScene: Scene = mock[Scene]
  val mockPhysics: PhysicsEngine = mock[PhysicsEngine]
  val mockRender: RenderEngine = mock[RenderEngine]

  test("a game loop should start in edit mode by default"):
    val gameLoop = GameLoop()
    gameLoop.mode shouldBe EditMode

  test("a game loop should be able to switch between edit mode and simulation mode"):
    val gameLoop = GameLoop()
    gameLoop.withMode(SimulationMode).mode shouldBe SimulationMode

  test("a game loop should be able to switch between simulation mode and edit mode"):
    val gameLoop = GameLoop()
    gameLoop.withMode(SimulationMode).withMode(EditMode).mode shouldBe EditMode

  test("our game loop should have a default tick period of 60Hz"):
    val gameLoop = GameLoop()
    gameLoop.tickTime shouldBe defaultTickTime

  test("a game loop should allow configuring a custom tick time"):
    val gameLoop = GameLoop()
    gameLoop.withTickTime(differentTickTime).tickTime shouldBe differentTickTime

  test("a game loop should not be running by default"):
    val gameLoop = GameLoop()
    gameLoop.isRunning shouldBe false

  test("a game loop can be started and stopped"):
    val gameLoop = GameLoop()
    val startedLoop = gameLoop.start()

    startedLoop.isRunning shouldBe true
    startedLoop.stop().isRunning shouldBe false

  test("if the game loop is not running, it should not update the physics"):
    val initialLoop = GameLoop()
    val currentTime = 1_000_000L

    mockPhysics.step.expects(*, *).never()
    mockRender.render.expects(*, *).anyNumberOfTimes()

    val (currentScene, _) = initialLoop.tick(mockScene, mockPhysics, mockRender, currentTime)

    currentScene shouldBe mockScene

  test("if the game loop is in edit mode, it should not update the physics"):
    val initialLoop = GameLoop().start()

    val currentTime = 1_000_000L

    mockPhysics.step.expects(*, *).never()
    mockRender.render.expects(*, *).anyNumberOfTimes()

    val (currentScene, _) = initialLoop.tick(mockScene, mockPhysics, mockRender, currentTime)

    currentScene shouldBe mockScene

  test("if the game loop is in edit mode or not running, it should still update its last timestamp"):
    val initialTime = 0L
    val currentTime = 30_000_000L

    val initialLoop = GameLoop(lastTime = initialTime)

    mockRender.render.expects(*, *).anyNumberOfTimes()

    val (_, currentLoop) = initialLoop.tick(mockScene, mockPhysics, mockRender, currentTime)

    currentLoop.lastTime shouldBe currentTime

  test("in simulation mode, passing less than one tick period should not invoke the physics engine"):
    val initialTime = 0L
    val timeDifference = 1L
    val currentTime = defaultTickTime - timeDifference

    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = initialTime)

    mockPhysics.step.expects(*, *).never()
    mockRender.render.expects(*, *).anyNumberOfTimes()

    val (currentScene, currentLoop) = initialLoop.tick(mockScene, mockPhysics, mockRender, currentTime)

    currentScene shouldBe mockScene
    currentLoop.lastTime shouldBe currentTime

  test("in simulation mode, passing exactly one tick period should invoke the physics engine once"):
    val updatedScene = mock[Scene]

    val initialTime = 0L
    val currentTime = defaultTickTime

    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = initialTime)

    mockPhysics.step.expects(mockScene, currentTime).returning(updatedScene).once()
    mockRender.render.expects(*, *).anyNumberOfTimes()

    val (currentScene, currentLoop) = initialLoop.tick(mockScene, mockPhysics, mockRender, currentTime)

    currentScene shouldBe updatedScene
    currentLoop.lastTime shouldBe currentTime

  test("in simulation mode, passing two tick periods should invoke the physics engine twice"):
    val sceneStep1 = mock[Scene]
    val sceneStep2 = mock[Scene]

    val initialTime = 0L
    val currentTime = defaultTickTime * 2

    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = initialTime)

    mockRender.render.expects(*, *).anyNumberOfTimes()

    inSequence {
      mockPhysics.step.expects(mockScene, defaultTickTime).returning(sceneStep1).once()
      mockPhysics.step.expects(sceneStep1, defaultTickTime).returning(sceneStep2).once()
    }

    val (currentScene, currentLoop) = initialLoop.tick(mockScene, mockPhysics, mockRender, currentTime)

    currentScene shouldBe sceneStep2
    currentLoop.lastTime shouldBe currentTime

  test("in simulation mode, remaining time after fixed updates must be saved in the accumulator"):
    val updatedScene = mock[Scene]

    val initialTime = 0L
    val currentTime = 20_000_000L
    val correctAccumulator = 4_000_000L

    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = initialTime)

    mockPhysics.step.expects(mockScene, defaultTickTime).returning(updatedScene).once()
    mockRender.render.expects(*, *).anyNumberOfTimes()

    val (_, currentLoop) = initialLoop.tick(mockScene, mockPhysics, mockRender, currentTime)

    currentLoop.accumulator shouldBe correctAccumulator

  test("massive lag spikes must be clamped to prevent overload"):
    val initialTime = 0L
    val currentTime = 1_000_000_000L
    val correctAccumulator = 10_000_000L

    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = initialTime, maxFrameTime = defaultMaxFrameTime)

    // 250ms di tempo bloccato / 16ms di tick = 15 iterazioni
    mockPhysics.step.expects(*, *).repeated(15)
    mockRender.render.expects(*, *).anyNumberOfTimes()

    val (_, currentLoop) = initialLoop.tick(mockScene, mockPhysics, mockRender, currentTime)

    currentLoop.accumulator shouldBe correctAccumulator

  test("game loop must invoke the render engine passing the correct interpolation alpha"):
    val updatedScene = mock[Scene]

    val initialTime = 0L
    val currentTime = 20_000_000L
    val correctAlpha = 0.25 // alpha = 4ms / 16ms = 0.25
    val correctAccumulator = 4_000_000L

    val initialLoop = GameLoop(mode = SimulationMode, isRunning = true, lastTime = initialTime)

    mockPhysics.step.expects(mockScene, defaultTickTime).returning(updatedScene).once()

    mockRender.render.expects(updatedScene, correctAlpha).once()

    val (_, currentLoop) = initialLoop.tick(mockScene, mockPhysics, mockRender, currentTime)

    currentLoop.accumulator shouldBe correctAccumulator

  test("game loop must invoke the render engine with static alpha when stopped"):
    val initialTime = 0L
    val currentTime = 20_000_000L
    val correctAlpha = 1.0

    val initialLoop = GameLoop(lastTime = initialTime)

    mockRender.render.expects(mockScene, correctAlpha).once()

    val (currentScene, currentLoop) = initialLoop.tick(mockScene, mockPhysics, mockRender, currentTime)

    currentScene shouldBe mockScene

  test("stopping or switching mode must freeze the simulation, which can then be resumed"):
    val updatedScene = mock[Scene]
    val initialTime = 0L
    val partialTime1 = 16_000_000L
    val partialTime2 = 32_000_000L
    val partialTime3 = 48_000_000L

    val loop0 = GameLoop(mode = SimulationMode, isRunning = true, lastTime = initialTime)

    mockRender.render.expects(*, *).anyNumberOfTimes()

    mockPhysics.step.expects(mockScene, defaultTickTime).returning(updatedScene).once()
    val (scene1, loop1) = loop0.tick(mockScene, mockPhysics, mockRender, partialTime1)

    val loopPaused = loop1.stop()
    mockPhysics.step.expects(*, *).never()
    val (scene2, loop2) = loopPaused.tick(scene1, mockPhysics, mockRender, partialTime2)

    scene2 shouldBe scene1

    val loopResumed = loop2.start()
    mockPhysics.step.expects(scene2, defaultTickTime).returning(updatedScene).once()
    val (scene3, finalLoop) = loopResumed.tick(scene2, mockPhysics, mockRender, partialTime3)

    scene3 shouldBe updatedScene
    finalLoop.isRunning shouldBe true