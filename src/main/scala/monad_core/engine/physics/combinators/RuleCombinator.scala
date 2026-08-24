package monad_core.engine.physics.combinators

import monad_core.engine.physics.core.{PhysicsContext, PhysicsError, PhysicsRule, PhysicsRuleResult}

private[physics] object RuleCombinator:

  def sequence(rules: Seq[PhysicsRule]): PhysicsRule =
    (context: PhysicsContext) =>
      rules.foldLeft[Either[PhysicsError, PhysicsRuleResult]](
        Right(PhysicsRuleResult(context.state))
      ): (currentResult, rule) =>
        currentResult.flatMap { accumulated =>
          rule(context.copy(state = accumulated.state)).map { next =>
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
