package monad_core.engine.physics.core

import monad_core.engine.core.events.EngineEvent
import monad_core.engine.core.traits.State

/**
 * Result produced by applying a physics rule.
 *
 * @param state
 *   state produced by the rule
 * @param events
 *   events produced by the rule
 */
final case class PhysicsRuleResult(
    state: State,
    events: Vector[EngineEvent] = Vector.empty
)

/** Immutable transformation applied to a physics context. */
trait PhysicsRule:

  /** Identifier used to compare named rules. */
  val RuleId = ""

  /**
   * Applies this rule to the supplied physics context.
   *
   * @param context
   *   state, elapsed time and detected contacts available to the rule
   * @return
   *   transformed state and generated events, or a [[PhysicsError]]
   */
  def apply(context: PhysicsContext): Either[PhysicsError, PhysicsRuleResult]

  /**
   * Compares named rules through their stable identifiers.
   *
   * @param obj
   *   value to compare with this rule
   * @return
   *   `true` when both values are rules with the same non-empty identifier
   */
  override def equals(obj: Any): Boolean =
    obj match
      case other: PhysicsRule if this.RuleId.nonEmpty =>
        this.RuleId == other.RuleId
      case _ => false

  /**
   * Computes a hash code consistent with identifier-based equality.
   *
   * @return
   *   hash code of the stable rule identifier
   */
  override def hashCode(): Int =
    RuleId.hashCode
