package monad_core.engine.model

/** Strictly positive weight associated with an [[Entity]]. */
opaque type Weight = Int

/** Creates and exposes validated [[Weight]] values. */
object Weight:

  /**
   * Creates a validated weight.
   *
   * @param w
   *   the weight value; it must be greater than zero
   * @return
   *   the weight, or a [[WeightCannotBeNegativeOrZero]] error
   */
  def apply(w: Int): Either[EngineError, Weight] =
    Either.cond(w > 0, w, WeightCannotBeNegativeOrZero())

  /**
   * Validates an optional raw weight.
   *
   * @param optionalWight raw weight, or `None` when weight is not configured
   * @return `Right(None)` when absent, `Right(Some(Weight))` for a positive value, or
   *   [[WeightCannotBeNegativeOrZero]] for a non-positive value
   */
  def fromOption(optionalWight: Option[Int]): Either[EngineError, Option[Weight]] =
    ModelUtils.optionalize(optionalWight, Weight(_))

  extension (w: Weight)
    /**
     * Returns the underlying weight value.
     *
     * @return strictly positive integer weight
     */
    def value: Int = w
