package engine.core

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalamock.scalatest.MockFactory

class GameLoopTest extends AnyFunSuite with Matchers with MockFactory :
  
  val defaultTickTime = 0.016
  val differentTickTime = 0.01
  
  test("a game loop should start in edit mode by default"):
    val gameLoop = GameLoop()
    gameLoop.mode shouldBe EditMode

  test("a game loop should be able to switch between edit mode and simulation mode"):
    val gameLoop = GameLoop()
    gameLoop.withMode(SimulationMode).mode shouldBe SimulationMode

  test("a game loop should be able to switch between simulation mode and edit mode"):
    val gameLoop = GameLoop()
    gameLoop.withMode(SimulationMode).withMode(EditMode).mode shouldBe EditMode

  test("our game loop should have a default tick time of 0.016 seconds"):
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