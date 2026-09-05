package monad_core.engine.helper

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.model.Vector2D
import org.scalamock.scalatest.MockFactory

/** Shared deterministic constants used by physics unit tests. */
private[engine] object PhysicsConstantHelper:

  /** Nanoseconds contained in one second. */
  val DeltaTimeOneSecond = 1_000_000_000L

  /** Representative invalid negative duration. */
  val NegativeDt = -1L

  /** Default radius used by circular physics fixtures. */
  val DefaultRadius = 1.0
