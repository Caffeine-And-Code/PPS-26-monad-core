package monad_core.engine.physics.core

import monad_core.engine.core.events.EngineEvent
import monad_core.engine.core.traits.State

/**
 * The record class returned by each rule application
 * 
 * @param state the new state produced by the rule
 * @param events the events produced by the rule
 */
final case class PhysicsRuleResult(
    state: State,
    events: Vector[EngineEvent] = Vector.empty
)

trait PhysicsRule:
  val RuleId = ""

  def apply(context: PhysicsContext): Either[PhysicsError, PhysicsRuleResult]

  override def equals(obj: Any): Boolean =
    obj match
      case other: PhysicsRule if this.RuleId.nonEmpty =>
        this.RuleId == other.RuleId
      case _ => false

  override def hashCode(): Int =
    RuleId.hashCode
