package monad_core.engine.model

object ModelUtils:

  def optionalize[A, B](
      optionalValue: Option[A],
      toEither: A => Either[EngineError, B]
  ): Either[EngineError, Option[B]] =
    optionalValue match
      case Some(value) => toEither(value).map(damageValue => Some(damageValue))
      case None        => Right(None)
