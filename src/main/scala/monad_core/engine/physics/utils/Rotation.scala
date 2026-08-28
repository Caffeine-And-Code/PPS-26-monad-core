package monad_core.engine.physics.utils

/** Pure degree-based rotation normalization and interpolation. */
private[engine] object Rotation:

  private val FullTurn      = 360.0
  private val HalfTurn      = FullTurn / 2.0
  private val ThreeHalfTurn = HalfTurn * 3.0

  /**
   * Normalizes an angle to the interval from zero included to 360 excluded.
   *
   * @param rotation
   *   angle in degrees
   * @return
   *   equivalent normalized angle
   */
  def normalize(rotation: Double): Double =
    val normalized = rotation % FullTurn
    if normalized < 0.0 then normalized + FullTurn else normalized

  /**
   * Interpolates along the shortest angular path between two rotations.
   * The wrapped delta prevents a transition such as 350 to 10 degrees from taking the long path.
   *
   * @param previous
   *   starting angle in degrees
   * @param next
   *   target angle in degrees
   * @param alpha
   *   interpolation ratio
   * @return
   *   normalized interpolated angle
   */
  def interpolate(previous: Double, next: Double, alpha: Double): Double =
    val delta = ((next - previous + ThreeHalfTurn) % FullTurn) - HalfTurn
    normalize(previous + delta * alpha)
