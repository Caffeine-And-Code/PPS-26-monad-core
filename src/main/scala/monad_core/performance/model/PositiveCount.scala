package monad_core.performance.model

private object PositiveCount:

  def from(name: String, value: Int): Either[PerformanceError, Int] =
    Either.cond(value > 0, value, InvalidPositiveCount(name, value))

opaque type EntityCount = Int

object EntityCount:

  def from(value: Int): Either[PerformanceError, EntityCount] =
    PositiveCount.from("Entity count", value)

  extension (count: EntityCount) def value: Int = count

opaque type IterationCount = Int

object IterationCount:

  def from(value: Int): Either[PerformanceError, IterationCount] =
    PositiveCount.from("Iteration count", value)

  extension (count: IterationCount) def value: Int = count
