package monad_core.simulator.presentation.stages.support

import monad_core.engine.errors.EngineError
import monad_core.simulator.InvalidSizeValue

private[simulator] case class Size(width: Double, height: Double)

object Size:
  private def validate(width: Double, height: Double): Either[EngineError, Unit] =
    if width < 0 || height < 0 then
      Left(InvalidSizeValue(width, height))
    else
      Right(())

  def square(edgeLength: Double): Either[EngineError, Size] =
    validate(edgeLength, edgeLength).map(_ => Size(edgeLength, edgeLength))

  def rectangle(width: Double, height: Double): Either[EngineError, Size] =
    validate(width, height).map(_ => Size(width, height))
    