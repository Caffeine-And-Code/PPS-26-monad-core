package monad_core.engine.physics.core

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.events.Event
import monad_core.engine.core.traits.State

final case class PhysicsRuleResult(
    state: State,
    events: Vector[Event] = Vector.empty
)

trait PhysicsRule:
  val RuleId = ""

  def apply(scene: State, dt: Long)(using
      detector: CollisionDetector
  ): Either[PhysicsError, PhysicsRuleResult]

  override def equals(obj: Any): Boolean =
    obj match
      case that: PhysicsRule if this.RuleId.nonEmpty && that.RuleId.nonEmpty =>
        this.RuleId == that.RuleId
      case _ => super.equals(obj)

  override def hashCode(): Int =
    if RuleId.nonEmpty then RuleId.hashCode else super.hashCode()
