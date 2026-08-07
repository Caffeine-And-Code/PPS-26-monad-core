package integrations.monad_core.simulator.infrastructure.engine

import integrations.monad_core.simulator.presentation.support.ScalaFxInit
import monad_core.engine.core.Scene
import monad_core.engine.model.{Entity, Vector2D}
import monad_core.engine.public_api.Painter
import monad_core.simulator.application.engine.world.{SaveEntityCommand, World}
import monad_core.simulator.domain.engine.MonadCoreEntity
import monad_core.simulator.domain.engine.MonadCoreShape.SimulationCircle
import monad_core.simulator.infrastructure.engine.{MonadCoreGameEngineRuntime, MonadCoreWorld}
import monad_core.simulator.presentation.painters.Drawer
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{CountDownLatch, TimeUnit}

class GameEngineTest extends AnyFunSuite with ScalaFxInit:

  given Painter = Drawer

  private val AwaitTimeout = 5L

  private val StarterEntityId = "starter"

  private def idsExcludingStarter(world: World): Set[String] =
    world.getAllEntities.map(_.id).toSet - StarterEntityId

  private def worldWithOneEntity(id: String): World =
    val entity = getOrFail(Entity.circle(id, Vector2D(0, 0), 1))
    val scene = Scene()
    val world = for
      newScene <- scene.addEntity(entity)
    yield MonadCoreWorld(newScene)

    getOrFail(world)

  test("init starts the loop and delivers frames through onFrame"):
    val firstFrame = new CountDownLatch(1)
    val received = new AtomicReference[World]()

    val engine = MonadCoreGameEngineRuntime()

    engine.initializeWorld(MonadCoreWorld(Scene()))
    engine.attach(world => {
      received.set(world)
      firstFrame.countDown()
    })

    assert(firstFrame.await(AwaitTimeout, TimeUnit.SECONDS), "onFrame was never called after init")
    received.get().getAllEntities.length should be(1)

  test("play and pause do not break the frame loop"):
    val frames = new CountDownLatch(3)

    val engine = MonadCoreGameEngineRuntime()

    engine.initializeWorld(MonadCoreWorld(Scene()))
    engine.attach(_ => frames.countDown())
    engine.start()
    engine.stop()
    engine.start()

    assert(frames.await(AwaitTimeout, TimeUnit.SECONDS), "engine stopped delivering frames after play/pause")


  test("reset replaces the world; frames observed afterwards reflect the new world, not the old one"):
    val worldBeforeReset = worldWithOneEntity("before")
    val worldAfterReset = worldWithOneEntity("after")

    val sawPreResetFrame = new CountDownLatch(1)
    val frameAfterReset = new CountDownLatch(1)
    val received = new AtomicReference[World]()

    val engine = MonadCoreGameEngineRuntime()

    engine.attach { world =>
      if world.getAllEntities == worldAfterReset.getAllEntities then
        received.set(world)
        frameAfterReset.countDown()
      else
        sawPreResetFrame.countDown()
    }

    engine.initializeWorld(worldBeforeReset)
    engine.start()

    val hasWaitedForAtLeastOneFrameBeforeReset = sawPreResetFrame.await(AwaitTimeout, TimeUnit.SECONDS)
    hasWaitedForAtLeastOneFrameBeforeReset should be(true)

    engine.initializeWorld(worldAfterReset)

    val hasWaitedForAtLeastOneFrameAfterReset = frameAfterReset.await(AwaitTimeout, TimeUnit.SECONDS)
    hasWaitedForAtLeastOneFrameAfterReset should be(true)

    received.get().getAllEntities should be(worldAfterReset.getAllEntities)

  test("resetToSnapshot restores the state captured by createSnapshot, discarding later changes"):
    val worldBefore = worldWithOneEntity("keeper")
    val extraEntity = MonadCoreEntity("intruder", (0, 0), SimulationCircle(1))

    val frameAfterReset = new CountDownLatch(1)
    val received = new AtomicReference[World]()

    val engine = MonadCoreGameEngineRuntime()

    engine.initializeWorld(worldBefore)
    engine.createSnapshot()

    worldBefore.createEntity(SaveEntityCommand(extraEntity))

    engine.attach { world =>
      if idsExcludingStarter(world) == Set("keeper") then
        received.set(world)
        frameAfterReset.countDown()
    }

    engine.resetToSnapshot()

    frameAfterReset.await(AwaitTimeout, TimeUnit.SECONDS) shouldBe true
    idsExcludingStarter(received.get()) shouldBe Set("keeper")

  test("resetToSnapshot without a prior createSnapshot does not crash and leaves the loop deliverable"):
    val frames = new CountDownLatch(1)
    val engine = MonadCoreGameEngineRuntime()

    engine.initializeWorld(worldWithOneEntity("solo"))
    engine.attach(_ => frames.countDown())

    noException should be thrownBy engine.resetToSnapshot()
    frames.await(AwaitTimeout, TimeUnit.SECONDS) shouldBe true

  test("resetToSnapshot stops the engine (isRunning becomes false)"):
    val engine = MonadCoreGameEngineRuntime()

    engine.initializeWorld(worldWithOneEntity("x"))
    engine.attach(_ => ())
    engine.start()

    engine.isRunning shouldBe true
    engine.resetToSnapshot()
    engine.isRunning shouldBe false

  test("createSnapshot called twice keeps only the latest snapshot"):
    val world = worldWithOneEntity("a")
    val entityB = MonadCoreEntity("b", (0, 0), SimulationCircle(1))
    val entityC = MonadCoreEntity("c", (0, 0), SimulationCircle(1))

    val frameAfterReset = new CountDownLatch(1)
    val received = new AtomicReference[World]()

    val engine = MonadCoreGameEngineRuntime()
    engine.initializeWorld(world)

    world.createEntity(SaveEntityCommand(entityB))
    engine.createSnapshot()

    world.createEntity(SaveEntityCommand(entityC))
    engine.createSnapshot()

    engine.attach { w =>
      if idsExcludingStarter(w) == Set("a", "b", "c") then
        received.set(w)
        frameAfterReset.countDown()
    }

    engine.resetToSnapshot()

    frameAfterReset.await(AwaitTimeout, TimeUnit.SECONDS) shouldBe true
    idsExcludingStarter(received.get()) shouldBe Set("a", "b", "c")

  test("stop after resetToSnapshot does not deliver further frames, and start resumes correctly"):
    val engine = MonadCoreGameEngineRuntime()
    val framesAfterExplicitStop = new AtomicReference(0)

    engine.initializeWorld(worldWithOneEntity("x"))
    engine.attach(_ => framesAfterExplicitStop.updateAndGet(_ + 1))
    engine.start()
    engine.resetToSnapshot()

    engine.isRunning shouldBe false

    engine.start()
    engine.isRunning shouldBe true