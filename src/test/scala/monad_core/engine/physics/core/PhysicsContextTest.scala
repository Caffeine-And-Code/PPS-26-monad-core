package monad_core.engine.physics.core

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.geometry.Collision
import monad_core.engine.helper.DummyEntityHelper.{makeFixedEntityCircle, makeMovingEntityCircle}
import monad_core.engine.helper.DummySurfaceHelper.makeSurfaceCircle
import monad_core.engine.helper.MockStateHelper
import monad_core.engine.helper.PhysicsConstantHelper.DeltaTimeOneSecond
import monad_core.engine.model.Vector2D
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PhysicsContextTest extends AnyFunSuite with Matchers with MockFactory with MockStateHelper:

  test("detect should build one immutable snapshot for entity and surface contacts"):
    val movingEntity = makeMovingEntityCircle("moving")
    val fixedEntity  = makeFixedEntityCircle("fixed")
    val surface      = makeSurfaceCircle()
    val collision    = Collision(Vector2D(1, 0), 2, Vector2D(0.5, 0))
    val state        = stateWithSurfaces(List(movingEntity, fixedEntity), List(surface))
    val detector     = mock[CollisionDetector]

    detector.collision.expects(movingEntity, fixedEntity).returning(Some(collision)).once()
    detector.isInside.expects(movingEntity, surface).returning(true).once()

    val context = PhysicsContext.detect(state, DeltaTimeOneSecond)(using detector)

    context shouldBe PhysicsContext(
      state = state,
      dt = DeltaTimeOneSecond,
      collisions = CollisionSnapshot(
        entityContacts = Vector(
          EntityCollisionContact(movingEntity.id, fixedEntity.id, collision)
        ),
        surfaceContacts = Vector(SurfaceContact(movingEntity.id, surface.id))
      )
    )

  test("detect should ignore fixed entity pairs"):
    val firstFixed  = makeFixedEntityCircle("first")
    val secondFixed = makeFixedEntityCircle("second")
    val state       = stateWithEntities(List(firstFixed, secondFixed))
    val detector    = mock[CollisionDetector]

    detector.collision.expects(*, *).never()

    PhysicsContext.detect(state, DeltaTimeOneSecond)(using detector).collisions shouldBe
      CollisionSnapshot()
