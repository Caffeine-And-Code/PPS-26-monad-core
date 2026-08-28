package monad_core.performance.model

import scala.annotation.tailrec

opaque type GrowthFactor = Int

object GrowthFactor:

  def from(value: Int): Either[PerformanceError, GrowthFactor] =
    Either.cond(value > 1, value, InvalidGrowthFactor(value))

  extension (factor: GrowthFactor) def value: Int = factor

final case class EntityGrowth private (
                                        start: EntityCount,
                                        maximum: EntityCount,
                                        factor: GrowthFactor
                                      ):

  def counts: Either[PerformanceError, Vector[EntityCount]] =
    @tailrec
    def generateGrowthCount(
              current: Int,
              accumulated: Vector[EntityCount]
            ): Either[PerformanceError, Vector[EntityCount]] =
      EntityCount.from(current) match
        case Left(error) => Left(error)
        case Right(count) =>
          val updated = accumulated :+ count
          if current == maximum.value then Right(updated)
          else
            val multiplied = current.toLong * factor.value.toLong
            val nextValue  = math.min(multiplied, maximum.value.toLong).toInt
            generateGrowthCount(nextValue, updated)

    generateGrowthCount(start.value, Vector.empty)

object EntityGrowth:

  def from(
            start: Int,
            maximum: Int,
            factor: Int
          ): Either[PerformanceError, EntityGrowth] =
    for
      startCount   <- EntityCount.from(start)
      maximumCount <- EntityCount.from(maximum)
      _ <- Either.cond(
        maximumCount.value >= startCount.value,
        (),
        InvalidGrowthMaximum(start, maximum)
      )
      growthFactor <- GrowthFactor.from(factor)
    yield EntityGrowth(startCount, maximumCount, growthFactor)
