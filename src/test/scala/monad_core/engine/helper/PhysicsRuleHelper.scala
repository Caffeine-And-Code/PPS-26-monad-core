package monad_core.engine.helper

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.events.Event
import monad_core.engine.core.traits.State
import monad_core.engine.physics.core.{PhysicsError, PhysicsRule, PhysicsRuleResult}

object PhysicsRuleHelper:

  def makeDummyRule(
      id: String = "rule-id",
      action: (State, Long) => Either[PhysicsError, State] = (_1, _2) => Right(_1),
      events: Vector[Event] = Vector.empty
  ): PhysicsRule =
    new PhysicsRule:
      override val RuleId: String = id
      override def apply(scene: State, dt: Long)(using
          detector: CollisionDetector
      ): Either[PhysicsError, PhysicsRuleResult] =
        action(scene, dt).map(PhysicsRuleResult(_, events))
