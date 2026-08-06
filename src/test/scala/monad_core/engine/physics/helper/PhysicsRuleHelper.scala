package monad_core.engine.physics.helper

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.physics.core.{PhysicsError, PhysicsRule}

object PhysicsRuleHelper :
  def makeDummyRule(id: String = "rule-id", action: (State, Long) => Either[PhysicsError, State]): PhysicsRule =
    new PhysicsRule:
      override val ruleId: String = id
      override def apply(scene: State, dt: Long)(using detector: CollisionDetector): Either[PhysicsError, State] =
        action(scene, dt)
