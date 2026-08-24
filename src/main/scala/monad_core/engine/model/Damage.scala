package monad_core.engine.model

opaque type Damage = Int

object Damage:

  def apply(damage: Int): Either[EngineError, Damage] =
    Either.cond(damage >= 0, damage, DamageCannotBeNegative())

  extension (damage: Damage) def value: Int = damage
