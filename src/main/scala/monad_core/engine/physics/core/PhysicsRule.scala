package monad_core.engine.physics.core

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State

trait PhysicsRule:
  val RuleId = ""
  def apply(scene: State, dt: Long)(using detector: CollisionDetector): Either[PhysicsError, State]

  override def equals(obj: Any): Boolean =
    obj match
      case other: PhysicsRule if this.RuleId.nonEmpty =>
        this.RuleId == other.RuleId
      case _ => false

  override def hashCode(): Int =
    RuleId.hashCode
