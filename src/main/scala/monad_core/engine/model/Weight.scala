package monad_core.engine.model

/** Strictly positive weight associated with an [[Entity]]. */
opaque type Weight = Int

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

  def fromOption(optionalWight: Option[Int]): Either[EngineError, Option[Weight]] =
    ModelUtils.optionalize(optionalWight, Weight(_))

  extension (w: Weight)
    /** Returns the underlying weight value. */
    def value: Int = w
