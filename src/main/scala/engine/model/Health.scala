package engine.model

opaque type Health = Int
object Health:
  def apply(h: Int): Either[String, Health] =
    Either.cond(h >= 0, h, s"Health cannot be negative: $h")

  extension (h: Health) def value: Int = h