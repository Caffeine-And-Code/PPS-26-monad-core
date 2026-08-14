package monad_core.simulator.infrastructure.engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HeadlessEngineControlTest extends AnyFunSuite with Matchers:

  test("a headless engine control is initially stopped"):
    val engineControl = HeadlessEngineControl()

    engineControl.isRunning shouldBe false

  test("can start a headless engine control"):
    val engineControl = HeadlessEngineControl()

    engineControl.start()

    engineControl.isRunning shouldBe true

  test("can stop a headless engine control"):
    val engineControl = HeadlessEngineControl()
    engineControl.start()

    engineControl.stop()

    engineControl.isRunning shouldBe false
