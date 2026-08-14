package monad_core.engine.model

opaque type LocatableId = String

object LocatableId:

  def apply(locatableId: String): Either[EngineError, LocatableId] =
    Either.cond(locatableId.trim.nonEmpty, locatableId.trim, LocatableIdCannotBeEmpty())

  extension (locatableId: LocatableId) def value: String = locatableId
