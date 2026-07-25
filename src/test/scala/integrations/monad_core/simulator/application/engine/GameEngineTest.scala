package integrations.monad_core.simulator.application.engine

import integrations.monad_core.simulator.presentation.support.ScalaFxInit
import monad_core.engine.core.Scene
import monad_core.engine.model.{Entity, Vector2D}
import monad_core.engine.public_api.Painter
import monad_core.simulator.application.engine.GameEngine
import monad_core.simulator.application.engine.world.{SaveEntityCommand, World}
import monad_core.simulator.presentation.painters.Drawer
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import java.util.concurrent.{CountDownLatch, TimeUnit}

class GameEngineTest extends AnyFunSuite with ScalaFxInit:

  given Painter = Drawer

  private val AwaitTimeout = 5L

  private def worldWithOneEntity(id: String): World =
    val entity = getOrFail(Entity.circle(id, Vector2D(0, 0), 1))
    getOrFail(World(Scene()).createEntity(SaveEntityCommand(entity)))

  test("init starts the loop and delivers frames through onFrame") {
    val firstFrame = new CountDownLatch(1)
    val received = new AtomicReference[World]()

    val engine = GameEngine(world => {
      received.set(world)
      firstFrame.countDown()
    })

    try
      engine.init(World(Scene()))

      assert(firstFrame.await(AwaitTimeout, TimeUnit.SECONDS), "onFrame was never called after init")
      assert(received.get().getAllEntities.isEmpty)
    finally
      engine.dispose()
  }

  test("play and pause do not break the frame loop") {
    val frames = new CountDownLatch(3)

    val engine = GameEngine(_ => frames.countDown())

    try
      engine.init(World(Scene()))
      engine.play()
      engine.pause()
      engine.play()

      assert(frames.await(AwaitTimeout, TimeUnit.SECONDS), "engine stopped delivering frames after play/pause")
    finally
      engine.dispose()
  }

