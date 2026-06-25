package engine.model

opaque type Health = Int

object Health:

  def apply(h: Int): Either[String, Health] =
    Either.cond(h > 0, h, s"Health cannot be negative or zero: $h")

  extension (h: Health)

    def value: Int = h

    private infix def inflict(damage: Int): Either[String, Health] = {
      if damage < 0 then
        Left("Cannot apply a negative damage")
      else
        Either.cond(h > damage, h-damage, "Health cannot be negative or zero")
    }

    def -(damage: Int): Either[String, Health] = h inflict damage