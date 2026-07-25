package engine.physics.combinators

import engine.physics.core.{PhysicsError, PhysicsRule}

object RuleCombinator:

  def sequence[S, CD](rules: Seq[PhysicsRule[S, CD]]): PhysicsRule[S, CD] =
    new PhysicsRule[S, CD]:
      override def apply(scene: S)(using detector: CD, dt: Long): Either[PhysicsError, S] =
        rules.foldLeft[Either[PhysicsError, S]](Right(scene)): (currentSceneResult, rule) =>
          currentSceneResult.flatMap(currentScene => rule(currentScene))

  extension [S, CD](self: PhysicsRule[S, CD])

    private infix def andThen(next: PhysicsRule[S, CD]): PhysicsRule[S, CD] =
      sequence(Seq(self, next))

    infix def +(next: PhysicsRule[S, CD]): PhysicsRule[S, CD] =
      self andThen next