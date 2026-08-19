package monad_core.engine.physics.helper

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.model.Vector2D
import org.scalamock.scalatest.MockFactory

private[physics] object PhysicsConstantHelper:

  val DeltaTimeOneSecond = 1_000_000_000L
  val NegativeDt         = -1L
  val DefaultRadius      = 1.0
  val DefaultDimension   = 1.0
