package monad_core.engine.physics.combinators

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.physics.core.{PhysicsError, PhysicsRule, PhysicsRuleResult}

private[physics] object RuleCombinator:

  def sequence(rules: Seq[PhysicsRule]): PhysicsRule =
    new PhysicsRule:
      override def apply(scene: State, dt: Long)(using
          detector: CollisionDetector
      ): Either[PhysicsError, PhysicsRuleResult] =
        rules.foldLeft[Either[PhysicsError, PhysicsRuleResult]](Right(PhysicsRuleResult(scene))):
          (currentResult, rule) =>
            currentResult.flatMap { accumulated =>
              rule(accumulated.state, dt).map { next =>
                PhysicsRuleResult(
                  state = next.state,
                  events = accumulated.events ++ next.events
                )
              }
            }

  extension (self: PhysicsRule)

    private infix def andThen(next: PhysicsRule): PhysicsRule =
      sequence(Seq(self, next))

    infix def +(next: PhysicsRule): PhysicsRule =
      self andThen next
