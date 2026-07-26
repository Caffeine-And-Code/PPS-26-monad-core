package integrations.monad_core.simulator.infrastructure.engine

import integrations.monad_core.simulator.presentation.support.ScalaFxInit
import monad_core.engine.core.Scene
import monad_core.engine.model.{Entity, Vector2D}
import monad_core.engine.public_api.Painter
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.infrastructure.engine.{MonadCodeGameEngineRuntime, MonadCoreWorld}
import monad_core.simulator.presentation.painters.Drawer
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import java.util.concurrent.{CountDownLatch, TimeUnit}

class GameEngineTest extends AnyFunSuite with ScalaFxInit:

  given Painter = Drawer

  private val AwaitTimeout = 5L

  private def worldWithOneEntity(id: String): World =
    val entity = getOrFail(Entity.circle(id, Vector2D(0, 0), 1))
    val scene = Scene()
    val world = for
      newScene <- scene.addEntity(entity)
    yield MonadCoreWorld(newScene)

    getOrFail(world)

  test("init starts the loop and delivers frames through onFrame") {
    val firstFrame = new CountDownLatch(1)
    val received = new AtomicReference[World]()

    val engine = MonadCodeGameEngineRuntime()

    engine.reset(MonadCoreWorld(Scene()))
    engine.attach(world => {
      received.set(world)
      firstFrame.countDown()
    })

    assert(firstFrame.await(AwaitTimeout, TimeUnit.SECONDS), "onFrame was never called after init")
    assert(received.get().getAllEntities.isEmpty)
  }

  test("play and pause do not break the frame loop") {
    val frames = new CountDownLatch(3)

    val engine = MonadCodeGameEngineRuntime()

    engine.reset(MonadCoreWorld(Scene()))
    engine.attach(_ => frames.countDown())
    engine.start()
    engine.stop()
    engine.start()

    assert(frames.await(AwaitTimeout, TimeUnit.SECONDS), "engine stopped delivering frames after play/pause")
  }

  test("reset replaces the world; frames observed afterwards reflect the new world, not the old one") {
    val worldBeforeReset = worldWithOneEntity("before")
    val worldAfterReset = worldWithOneEntity("after")

    val resetHappened = new AtomicBoolean(false)
    val frameAfterReset = new CountDownLatch(1)
    val received = new AtomicReference[World]()

    val engine = MonadCodeGameEngineRuntime()

    engine.attach(
      world =>
        if resetHappened.get() then
          received.set(world)
          frameAfterReset.countDown()
    )

    engine.reset(worldBeforeReset)
    engine.start()
    Thread.sleep(100) // let a few real frames run against the pre-reset world

    engine.reset(worldAfterReset)
    resetHappened.set(true)

    assert(frameAfterReset.await(AwaitTimeout, TimeUnit.SECONDS), "no frame observed after reset")
    assert(received.get().getAllEntities == worldAfterReset.getAllEntities)
  }
