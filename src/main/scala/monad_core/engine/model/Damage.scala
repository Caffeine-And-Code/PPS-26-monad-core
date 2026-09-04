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

  /**
   * Returns an optional raw damage amount.
   *
   * @param optionalDamage raw damage amount, or `None` when damage is not configured
   * @return `Right(None)` when absent, `Right(Some(Damage))` for a non-negative amount, or
   *   `Left(DamageCannotBeNegative)` for a negative amount
   */
  def fromOption(optionalDamage: Option[Int]): Either[EngineError, Option[Damage]] =
    ModelUtils.optionalize(optionalDamage, Damage(_))

  extension (damage: Damage)
    /** Returns the underlying non-negative integer value. */
    def value: Int = damage
