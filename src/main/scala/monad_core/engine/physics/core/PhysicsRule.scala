package monad_core.engine.physics.core

trait PhysicsRule[S, CD]:
  def apply(scene: S)(using detector: CD, dt: Long): Either[PhysicsError, S]