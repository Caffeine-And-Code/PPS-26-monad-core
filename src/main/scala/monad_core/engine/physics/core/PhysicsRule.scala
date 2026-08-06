package monad_core.engine.physics.core

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State

private[physics] trait PhysicsRule:
  val ruleId = ""
  def apply(scene: State, dt: Long)(using detector: CollisionDetector): Either[PhysicsError, State]

  override def equals(obj: Any): Boolean = obj match
    case that: PhysicsRule if this.ruleId.nonEmpty && that.ruleId.nonEmpty =>
      this.ruleId == that.ruleId
    case _ => super.equals(obj)

  override def hashCode(): Int =
    if ruleId.nonEmpty then ruleId.hashCode else super.hashCode()