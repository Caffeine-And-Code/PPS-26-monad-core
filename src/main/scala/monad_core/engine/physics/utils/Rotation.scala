package monad_core.engine.physics.utils

private[engine] object Rotation:

  private val FullTurn      = 360.0
  private val HalfTurn      = FullTurn / 2.0
  private val ThreeHalfTurn = HalfTurn * 3.0

  def normalize(rotation: Double): Double =
    val normalized = rotation % FullTurn
    if normalized < 0.0 then normalized + FullTurn else normalized

  def interpolate(previous: Double, next: Double, alpha: Double): Double =
    val delta = ((next - previous + ThreeHalfTurn) % FullTurn) - HalfTurn
    normalize(previous + delta * alpha)
