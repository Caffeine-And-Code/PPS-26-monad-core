package monad_core.engine.model

/**
 * Non-negative amount of damage that can be inflicted by an engine element.
 *
 * Values can be created safely through [[Damage.apply]].
 */
opaque type Damage = Int

/** Validated constructor and operations for [[Damage]]. */
object Damage:

  /**
   * Creates a damage value when the provided amount is non-negative.
   *
   * @param damage raw damage amount
   * @return `Right(Damage)` for a non-negative amount, or `Left(DamageCannotBeNegative)` otherwise
   */
  def apply(damage: Int): Either[EngineError, Damage] =
    Either.cond(damage >= 0, damage, DamageCannotBeNegative())

  extension (damage: Damage)
    /** Returns the underlying non-negative integer value. */
    def value: Int = damage
