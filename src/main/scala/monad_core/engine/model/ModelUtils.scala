package monad_core.engine.model

/** Shared utils for model elements. */
object ModelUtils:

  /**
   * Applies a validating conversion to an optional value.
   *
   * @param optionalValue source value, or `None` when the property is absent
   * @param toEither conversion that validates a present source value
   * @tparam A source value type
   * @tparam B validated destination type
   * @return `Right(None)` for an absent source, the converted value wrapped in `Some`, or the conversion error
   */
  def optionalize[A, B](
      optionalValue: Option[A],
      toEither: A => Either[EngineError, B]
  ): Either[EngineError, Option[B]] =
    optionalValue match
      case Some(value) => toEither(value).map(Some(_))
      case None        => Right(None)
