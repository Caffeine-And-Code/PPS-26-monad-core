package engine.model

opaque type Weight = Int

object Weight:
  def apply(w: Int): Either[String, Weight] =
    Either.cond(w >= 0, w, "Weight cannot be negative")
  extension (w: Weight) def value: Int = w