package engine.model

opaque type LocatableId = String

object LocatableId:

  def apply(locatableId: String): Either[String, LocatableId] =
    Either.cond(locatableId.trim.nonEmpty, locatableId.trim, "LocatableId cannot be empty")

  extension (locatableId: LocatableId)

    def value: String = locatableId