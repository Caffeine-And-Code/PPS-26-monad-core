package monad_core.engine.model

import scala.math

/**
 * Two-dimensional vector in Cartesian coordinates.
 *
 * @param x horizontal component
 * @param y vertical component
 */
case class Vector2D(x: Double, y: Double)

extension (value: Double)

  /**
   * Raises this value to a power.
   *
   * @param other exponent
   * @return `value` raised to `other`
   */
  def **(other: Double): Double =
    math.pow(value, other)

extension (v: Vector2D)

  /**
   * Adds another vector component-wise.
   *
   * @param toAdd vector to add
   * @return vector containing the sums of the corresponding components
   */
  infix def add(toAdd: Vector2D): Vector2D =
    Vector2D(v.x + toAdd.x, v.y + toAdd.y)

  /**
   * Subtracts another vector component-wise.
   *
   * @param toSub vector to subtract
   * @return vector containing the differences of the corresponding components
   */
  infix def sub(toSub: Vector2D): Vector2D =
    Vector2D(v.x - toSub.x, v.y - toSub.y)

  /**
   * Multiplies both components by a scalar.
   *
   * @param scalar multiplier
   * @return scaled vector
   */
  infix def times(scalar: Double): Vector2D =
    Vector2D(v.x * scalar, v.y * scalar)

  /**
   * Computes the dot product with another vector.
   *
   * @param toDot other vector
   * @return scalar dot product
   */
  infix def dot(toDot: Vector2D): Double =
    v.x * toDot.x + v.y * toDot.y

  /**
   * Computes the scalar two-dimensional cross product with another vector.
   *
   * @param toCross other vector
   * @return signed scalar cross product
   */
  infix def cross(toCross: Vector2D): Double =
    v.x * toCross.y - v.y * toCross.x

  /**
   * Computes the squared Euclidean distance from another point.
   *
   * @param other destination point
   * @return squared distance, without performing a square root
   */
  infix def squaredDistance(other: Vector2D): Double =
    ((v.x - other.x) ** 2) + ((v.y - other.y) ** 2)

  /**
   * Computes the Euclidean distance from another point.
   *
   * @param other destination point
   * @return non-negative Euclidean distance
   */
  infix def euclideanDistance(other: Vector2D): Double =
    math.sqrt(squaredDistance(other))

  /** @return non-negative Euclidean magnitude of this vector */
  def magnitude: Double =
    math.sqrt((v.x ** 2) + (v.y ** 2))

  /** @return unit vector with the same direction, or the zero vector when this vector has zero magnitude */
  def normalized: Vector2D =
    val currentMagnitude = v.magnitude

    if currentMagnitude == 0 then Vector2D(0, 0)
    else Vector2D(v.x / currentMagnitude, v.y / currentMagnitude)

  /** @return vector with both components negated */
  def flip: Vector2D =
    v.copy(-v.x, -v.y)

  /** @return vector with the horizontal and vertical components exchanged */
  def swap: Vector2D =
    v.copy(v.y, v.x)

  private infix def rotatedX(cosSin: Vector2D): Double =
    v.x * cosSin.x - v.y * cosSin.y

  /**
   * Rotates the vector counterclockwise around the origin.
   *
   * @param angle rotation in degrees
   * @return rotated vector
   */
  def rotated(angle: Double): Vector2D =
    val radians = math.toRadians(angle)
    val cosSin  = Vector2D(math.cos(radians), math.sin(radians))
    val sinCos  = cosSin.swap
    Vector2D(
      v rotatedX cosSin,
      v dot sinCos
    )

  /**
   * Operator alias for [[add]].
   *
   * @param toAdd vector to add
   * @return component-wise sum
   */
  def +(toAdd: Vector2D): Vector2D = v add toAdd

  /**
   * Operator alias for [[sub]].
   *
   * @param toSub vector to subtract
   * @return component-wise difference
   */
  def -(toSub: Vector2D): Vector2D = v sub toSub

  /**
   * Operator alias for [[times]].
   *
   * @param scalar multiplier
   * @return scaled vector
   */
  def *(scalar: Double): Vector2D = v times scalar

  /**
   * Operator alias for [[squaredDistance]].
   *
   * @param other destination point
   * @return squared Euclidean distance
   */
  def -->>(other: Vector2D): Double = v squaredDistance other

  /**
   * Operator alias for [[euclideanDistance]].
   *
   * @param other destination point
   * @return Euclidean distance
   */
  def -->(other: Vector2D): Double = v euclideanDistance other
