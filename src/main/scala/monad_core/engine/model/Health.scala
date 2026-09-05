package monad_core.engine.model

/** Strictly positive health value associated with an [[Entity]]. */
opaque type Health = Int

/** Creates, converts, and updates validated [[Health]] values. */
object Health:

  /**
   * Creates a validated health value.
   *
   * @param h the health points; they must be greater than zero
   * @return the health value, or a [[HealthCannotBeNegativeOrZero]] error
   */
  def apply(h: Int): Either[EngineError, Health] =
    Either.cond(h > 0, h, HealthCannotBeNegativeOrZero(h))

  /**
   * Returns an optional raw health value.
   *
   * @param optionalHealth raw health, or `None` when health is not configured
   * @return `Right(None)` when absent, `Right(Some(Health))` for a positive value, or
   *   [[HealthCannotBeNegativeOrZero]] for a non-positive value
   */
  def fromOption(optionalHealth: Option[Int]): Either[EngineError, Option[Health]] =
    ModelUtils.optionalize(optionalHealth, Health(_))

  extension (h: Health)

    /**
     * Returns the underlying health points.
     *
     * @return strictly positive health value
     */
    def value: Int = h

    private infix def inflict(damage: Int): Either[EngineError, Health] =
      if damage < 0 then Left(CannotApplyNegativeDamage(damage))
      else Health(h.value - damage)

    /**
     * Applies non-negative damage and returns the remaining health.
     *
     * @param damage the health points to subtract
     * @return the remaining health, or an error if damage is negative or leaves a non-positive value
     */
    def -(damage: Int): Either[EngineError, Health] = h inflict damage
