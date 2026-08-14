package monad_core.engine.model

opaque type Weight = Int

object Weight:

  def apply(w: Int): Either[EngineError, Weight] =
    Either.cond(w > 0, w, WeightCannotBeNegativeOrZero())

  extension (w: Weight) def value: Int = w
