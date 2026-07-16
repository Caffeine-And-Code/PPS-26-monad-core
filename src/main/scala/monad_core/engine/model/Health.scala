package monad_core.engine.model

import monad_core.engine.errors.EngineError

opaque type Health = Int

object Health:

  def apply(h: Int): Either[EngineError, Health] =
    Either.cond(h > 0, h, HealthCannotBeNegativeOrZero(h))

  extension (h: Health)

    def value: Int = h

    private infix def inflict(damage: Int): Either[EngineError, Health] =
      if damage < 0 then
        Left(CannotApplyNegativeDamage(damage))
      else
        Health(h.value - damage)

    def -(damage: Int): Either[EngineError, Health] = h inflict damage