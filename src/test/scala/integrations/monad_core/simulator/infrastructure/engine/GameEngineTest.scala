package integrations.monad_core.simulator.infrastructure.engine

import integrations.monad_core.simulator.presentation.support.ScalaFxInit
import monad_core.engine.core.events.EngineEvent
import monad_core.engine.core.events.EngineEvent.EntityUpdated
import monad_core.engine.core.{CannotAddAlreadyPresentElementInMap, CannotAddEntity, GameLoop}
import monad_core.engine.model.{Entity, Scene, Vector2D}
import monad_core.engine.simulator.Painter
import monad_core.simulator.application.engine.{DrawCommand, GameEngineRuntime}
import monad_core.simulator.application.engine.errors.EngineErrorAdapted
import monad_core.simulator.application.engine.world.{SaveEntityCommand, World}
import monad_core.simulator.infrastructure.engine.painters.PaintArchitect
import monad_core.simulator.infrastructure.engine.world.SceneEditingNotAllowed
import monad_core.simulator.infrastructure.engine.{MonadCoreGameEngineRuntime, MonadCoreWorld}
import org.scalatest.funsuite.AnyFunSuite
import scalafx.animation.AnimationTimer

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{CountDownLatch, TimeUnit}

class GameEngineTest extends AnyFunSuite with ScalaFxInit:

  given painter: Painter = PaintArchitect

  private val AwaitTimeout = 5L

  private def worldWithOneEntity(id: String): World =
    val entity = getOrFail(Entity.circle(id, Vector2D(0, 0), 1))
    val scene  = Scene()
    val world =
      for newScene <- scene.addEntity(entity)
      yield MonadCoreWorld(newScene)

    getOrFail(world)

  test("init starts the loop and delivers frames through onFrame"):
    val firstFrame = new CountDownLatch(1)
    val received   = new AtomicReference[World]()

    val engine = MonadCoreGameEngineRuntime()

    getOrFail(engine.initializeWorld(MonadCoreWorld(Scene())))
    engine.createSnapshot()
    val animationTimer = AnimationTimer { currentTime =>
      engine.tick(currentTime) { world =>
        received.set(world)
        firstFrame.countDown()
      }
    }
    animationTimer.start()

    assert(firstFrame.await(AwaitTimeout, TimeUnit.SECONDS), "onFrame was never called after init")
    received.get().getAllEntities.length should be(1)

  test("the runtime port adds the default entity when the option is omitted"):
    val world                     = MonadCoreWorld(Scene())
    val engine: GameEngineRuntime = MonadCoreGameEngineRuntime()

    getOrFail(engine.initializeWorld(world))

    world.getAllEntities.map(_.id.value) should contain("starter")

  test("play and pause do not break the frame loop"):
    val frames = new CountDownLatch(3)

    val engine = MonadCoreGameEngineRuntime()

    getOrFail(engine.initializeWorld(MonadCoreWorld(Scene())))
    engine.createSnapshot()
    val animationTimer = AnimationTimer { currentTime =>
      engine.tick(currentTime)(_ => frames.countDown())
    }
    animationTimer.start()
    engine.start()
    engine.stop()
    engine.start()

    assert(
      frames.await(AwaitTimeout, TimeUnit.SECONDS),
      "engine stopped delivering frames after play/pause"
    )

  test("a simulation tick commits the final state and publishes emitted events"):
    val movingEntity = getOrFail(
      Entity
        .circle("moving", Vector2D(10, 10), 1)
        .map(_.withSpeed(Vector2D(1, 0)))
    )
    val initialScene   = getOrFail(Scene().addEntity(movingEntity))
    val world          = MonadCoreWorld(initialScene)
    val receivedEvents = new AtomicReference(Vector.empty[EngineEvent])
    val engine = MonadCoreGameEngineRuntime(
      onEvents = events => receivedEvents.set(events)
    )

    getOrFail(engine.initializeWorld(world, withDefaultEntity = false))
    engine.start()
    engine.tick(GameLoop.DefaultTickTime)(_ => ())

    val updatedEntity = getOrFail(world.getEntity(movingEntity.id.value))
    updatedEntity.position.x should be > movingEntity.position.x
    receivedEvents.get() shouldBe Vector(EntityUpdated(movingEntity, updatedEntity))

  test("a physics rule can be disabled through the runtime"):
    val engine = MonadCoreGameEngineRuntime()
    val rule   = engine.physicsRules.head

    engine.setPhysicsRuleEnabled(rule.id, isEnabled = false)

    engine.physicsRules.find(_.id == rule.id).map(_.isEnabled) shouldBe Some(false)

  test("starting the runtime prevents edits on its world"):
    val entity = getOrFail(Entity.circle("new", Vector2D(0, 0), 1))
    val world  = MonadCoreWorld(Scene())
    val engine = MonadCoreGameEngineRuntime()

    getOrFail(engine.initializeWorld(world, withDefaultEntity = false))
    engine.start()
    val result = world.createEntity(SaveEntityCommand(entity))

    result shouldBe Left(SceneEditingNotAllowed)

  test("stopping the runtime enables edits on its world"):
    val entity = getOrFail(Entity.circle("new", Vector2D(0, 0), 1))
    val world  = MonadCoreWorld(Scene())
    val engine = MonadCoreGameEngineRuntime()

    getOrFail(engine.initializeWorld(world, withDefaultEntity = false))
    engine.start()
    engine.stop()
    val result = world.createEntity(SaveEntityCommand(entity))

    result shouldBe Right(())

  test("a world initialized while the runtime is running rejects edits"):
    val entity = getOrFail(Entity.circle("new", Vector2D(0, 0), 1))
    val world  = MonadCoreWorld(Scene())
    val engine = MonadCoreGameEngineRuntime()

    engine.start()
    getOrFail(engine.initializeWorld(world, withDefaultEntity = false))
    val result = world.createEntity(SaveEntityCommand(entity))

    result shouldBe Left(SceneEditingNotAllowed)

  test("world initialization propagates an error from the default entity creation"):
    val starter = getOrFail(Entity.circle("starter", Vector2D(15, 15), 15))
    val scene   = getOrFail(Scene().addEntity(starter))
    val engine  = MonadCoreGameEngineRuntime()

    val result = engine.initializeWorld(MonadCoreWorld(scene))

    result shouldBe Left(
      EngineErrorAdapted(CannotAddEntity(CannotAddAlreadyPresentElementInMap(starter.id)))
    )

  test("resetting the runtime enables edits on its world"):
    val entity = getOrFail(Entity.circle("new", Vector2D(0, 0), 1))
    val world  = MonadCoreWorld(Scene())
    val engine = MonadCoreGameEngineRuntime()

    getOrFail(engine.initializeWorld(world, withDefaultEntity = false))
    engine.createSnapshot()
    engine.start()
    engine.resetToSnapshot()
    val result = world.createEntity(SaveEntityCommand(entity))

    result shouldBe Right(())

  test("rendering after multiple fixed updates uses the state preceding the latest update"):
    PaintArchitect.drainBuffer()
    val movingEntity = getOrFail(
      Entity
        .circle("interpolated", Vector2D(10, 10), 1)
        .map(_.withSpeed(Vector2D(1, 0)))
    )
    val initialScene = getOrFail(Scene().addEntity(movingEntity))
    val world        = MonadCoreWorld(initialScene)
    val engine       = MonadCoreGameEngineRuntime()

    getOrFail(engine.initializeWorld(world, withDefaultEntity = false))
    engine.start()
    engine.tick(GameLoop.DefaultTickTime * 2)(_ => ())

    val simulatedX = getOrFail(world.getEntity(movingEntity.id.value)).position.x
    val renderedX = PaintArchitect
      .drainBuffer()
      .collectFirst { case DrawCommand.Circle(x, _, _, _) =>
        x
      }
      .getOrElse(fail("the interpolated entity was not rendered"))

    renderedX should be > movingEntity.position.x
    renderedX should be < simulatedX

  test(
    "reset replaces the world; frames observed afterwards reflect the new world, not the old one"
  ):
    val worldBeforeReset = worldWithOneEntity("before")
    val worldAfterReset  = worldWithOneEntity("after")

    val sawPreResetFrame = new CountDownLatch(1)
    val frameAfterReset  = new CountDownLatch(1)
    val received         = new AtomicReference[World]()

    val engine = MonadCoreGameEngineRuntime()

    getOrFail(engine.initializeWorld(worldBeforeReset, false))
    engine.createSnapshot()

    val animationTimer = AnimationTimer { currentTime =>
      engine.tick(currentTime) { world =>
        if world.getAllEntities == worldAfterReset.getAllEntities then
          received.set(world)
          frameAfterReset.countDown()
        else sawPreResetFrame.countDown()
      }
    }
    animationTimer.start()

    engine.resetToSnapshot()
    engine.start()

    val hasWaitedForAtLeastOneFrameBeforeReset =
      sawPreResetFrame.await(AwaitTimeout, TimeUnit.SECONDS)
    hasWaitedForAtLeastOneFrameBeforeReset should be(true)

    getOrFail(engine.initializeWorld(worldAfterReset, false))
    engine.createSnapshot()
    engine.resetToSnapshot()

    val hasWaitedForAtLeastOneFrameAfterReset =
      frameAfterReset.await(AwaitTimeout, TimeUnit.SECONDS)
    hasWaitedForAtLeastOneFrameAfterReset should be(true)

    received.get().getAllEntities should be(worldAfterReset.getAllEntities)
