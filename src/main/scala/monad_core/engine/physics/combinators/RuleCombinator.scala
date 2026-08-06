package monad_core.engine.physics.combinators

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.physics.core.{PhysicsError, PhysicsRule}

private[physics] object RuleCombinator:

  def sequence(rules: Seq[PhysicsRule]): PhysicsRule =
    new PhysicsRule:
      override def apply(scene: State, dt: Long)(using detector: CollisionDetector): Either[PhysicsError, State] =
        rules.foldLeft[Either[PhysicsError, State]](Right(scene)): (currentSceneResult, rule) =>
          currentSceneResult.flatMap(currentScene => rule(currentScene, dt))

  extension (self: PhysicsRule)

    private infix def andThen(next: PhysicsRule): PhysicsRule =
      sequence(Seq(self, next))

    infix def +(next: PhysicsRule): PhysicsRule =
      self andThen next