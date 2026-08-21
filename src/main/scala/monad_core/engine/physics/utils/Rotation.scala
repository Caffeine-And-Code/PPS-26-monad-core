package monad_core.engine.physics.utils

object Rotation:

  private val FullTurn = 360.0
  private val HalfTurn = FullTurn / 2.0

  def normalize(rotation: Double): Double =
    val normalized = rotation % FullTurn
    if normalized < 0.0 then normalized + FullTurn else normalized

  def interpolate(previous: Double, next: Double, alpha: Double): Double =
    val delta = ((next - previous + HalfTurn * 3.0) % FullTurn) - HalfTurn
    normalize(previous + delta * alpha)
