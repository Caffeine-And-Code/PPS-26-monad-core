package monad_core.engine.physics.combinators

import monad_core.engine.physics.core.{PhysicsContext, PhysicsError, PhysicsRule, PhysicsRuleResult}

/** Functional composition utilities for physics rules. */
private[physics] object RuleCombinator:

  /**
    * Combines rules into a single left-to-right state transformation.
    * Events are accumulated in execution order and the first error stops the sequence.
    *
    * @param rules
    *   ordered rules to apply
    * @return
    *   rule representing the complete sequence
    */
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

    /**
      * Composes this rule with a following rule.
      *
      * @param next
      *   rule applied to the state produced by this rule
      * @return
      *   composed rule
      */
    private infix def andThen(next: PhysicsRule): PhysicsRule =
      sequence(Seq(self, next))

    /**
      *  Composes this rule with a following rule.
      *
      * @param next
      *   rule applied to the state produced by this rule
      * @return
      *   composed rule
      */
    infix def +(next: PhysicsRule): PhysicsRule =
      self andThen next
