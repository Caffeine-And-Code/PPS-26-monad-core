package monad_core.engine.helper

import monad_core.engine.core.events.EngineEvent
import monad_core.engine.core.traits.State
import monad_core.engine.physics.core.{PhysicsContext, PhysicsError, PhysicsRule, PhysicsRuleResult}

object PhysicsRuleHelper:

  def makeDummyRule(
      id: String = "rule-id",
      action: (State, Long) => Either[PhysicsError, State] = (_1, _2) => Right(_1),
      events: Vector[EngineEvent] = Vector.empty
  ): PhysicsRule =
    new PhysicsRule:
      override val RuleId: String = id
      override def apply(context: PhysicsContext): Either[PhysicsError, PhysicsRuleResult] =
        action(context.state, context.dt).map(PhysicsRuleResult(_, events))
