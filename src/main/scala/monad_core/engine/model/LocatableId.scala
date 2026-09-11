package monad_core.engine.model

/** Validated, non-empty identifier of a [[Locatable]]. */
opaque type LocatableId = String

/** Creates and exposes validated [[LocatableId]] values. */
object LocatableId:

  /**
   * Creates an identifier after trimming leading and trailing whitespace.
   *
   * @param locatableId
   *   the identifier to validate
   * @return
   *   the trimmed identifier, or a [[LocatableIdCannotBeEmpty]] error
   */
  def apply(locatableId: String): Either[EngineError, LocatableId] =
    Either.cond(locatableId.trim.nonEmpty, locatableId.trim, LocatableIdCannotBeEmpty())

  extension (locatableId: LocatableId)
    /**
     * Returns the underlying identifier.
     *
     * @return trimmed, non-empty identifier
     */
    def value: String = locatableId
