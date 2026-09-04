package monad_core.performance.model

/** Shared validation for positive integer counts. */
private object PositiveCount:

  /**
   * Validates that a named count is greater than zero.
   *
   * @param name
   *   count name included in a validation error
   * @param value
   *   integer to validate
   * @return
   *   `value` when positive, or a validation error otherwise
   * @see
   *   [[monad_core.performance.model.InvalidPositiveCount InvalidPositiveCount]]
   */
  def from(name: String, value: Int): Either[PerformanceError, Int] =
    Either.cond(value > 0, value, InvalidPositiveCount(name, value))

/** Validated positive number of entities. */
opaque type EntityCount = Int

/** Provides validated construction and operations for entity counts. */
object EntityCount:

  /**
   * Creates a positive entity count.
   *
   * @param value
   *   entity count to validate
   * @return
   *   the validated count, or an error when `value` is not positive
   */
  def from(value: Int): Either[PerformanceError, EntityCount] =
    PositiveCount.from("Entity count", value)

  extension (count: EntityCount) def value: Int = count

/** Validated positive number of measured executions. */
opaque type IterationCount = Int

/** Provides validated construction and operations for iteration counts. */
object IterationCount:

  /**
   * Creates a positive measured-iteration count.
   *
   * @param value
   *   iteration count to validate
   * @return
   *   the validated count, or an error when `value` is not positive
   */
  def from(value: Int): Either[PerformanceError, IterationCount] =
    PositiveCount.from("Iteration count", value)

  extension (count: IterationCount) def value: Int = count
