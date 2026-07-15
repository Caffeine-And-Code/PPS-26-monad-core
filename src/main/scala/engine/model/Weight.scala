package engine.model

import engine.errors.EngineError

opaque type Weight = Int

object Weight:
  def apply(w: Int): Either[EngineError, Weight] =
    Either.cond(w >= 0, w, WeightCannotBeNegative())