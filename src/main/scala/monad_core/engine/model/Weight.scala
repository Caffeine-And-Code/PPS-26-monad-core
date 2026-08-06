package monad_core.engine.model

import monad_core.engine.errors.EngineError

opaque type Weight = Int

object Weight:
  def apply(w: Int): Either[EngineError, Weight] =
    Either.cond(w >= 0, w, WeightCannotBeNegative())

  extension (weight: Weight)

    def value: Int = weight