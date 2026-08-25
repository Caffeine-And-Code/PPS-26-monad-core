package monad_core.engine.model

opaque type Damage = Int

/**
 * Damage companion object which will validate the provided value according to the domain logic
 */
object Damage:

  def apply(damage: Int): Either[EngineError, Damage] =
    Either.cond(damage >= 0, damage, DamageCannotBeNegative())

  extension (damage: Damage) def value: Int = damage
