package monad_core.engine.model

import scala.math

/** Two-dimensional vector in Cartesian coordinates. */
case class Vector2D(x: Double, y: Double)

extension (value: Double)

  /** Raises this value to `other`. */
  def **(other: Double): Double =
    math.pow(value, other)

extension (v: Vector2D)

  /** Adds `toAdd` component-wise. */
  infix def add(toAdd: Vector2D): Vector2D =
    Vector2D(v.x + toAdd.x, v.y + toAdd.y)

  /** Subtracts `toSub` component-wise. */
  infix def sub(toSub: Vector2D): Vector2D =
    Vector2D(v.x - toSub.x, v.y - toSub.y)

  /** Multiplies both components by `scalar`. */
  infix def times(scalar: Double): Vector2D =
    Vector2D(v.x * scalar, v.y * scalar)

  /** Computes the dot product with `toDot`. */
  infix def dot(toDot: Vector2D): Double =
    v.x * toDot.x + v.y * toDot.y

  /** Computes the scalar two-dimensional cross product with `toCross`. */
  infix def cross(toCross: Vector2D): Double =
    v.x * toCross.y - v.y * toCross.x

  /** Computes the squared Euclidean distance from `other`. */
  infix def squaredDistance(other: Vector2D): Double =
    ((v.x - other.x) ** 2) + ((v.y - other.y) ** 2)

  /** Computes the Euclidean distance from `other`. */
  infix def euclideanDistance(other: Vector2D): Double =
    math.sqrt(squaredDistance(other))

  /** Returns the vector magnitude. */
  def magnitude: Double =
    math.sqrt((v.x ** 2) + (v.y ** 2))

  /** Returns the unit vector, or the zero vector when this vector is zero. */
  def normalized: Vector2D =
    val currentMagnitude = v.magnitude

    if currentMagnitude == 0 then Vector2D(0, 0)
    else Vector2D(v.x / currentMagnitude, v.y / currentMagnitude)

  /** Negates both components. */
  def flip: Vector2D =
    v.copy(-v.x, -v.y)

  /** Exchanges the x and y components. */
  def swap: Vector2D =
    v.copy(v.y, v.x)

  private infix def rotatedX(cosSin: Vector2D): Double =
    v.x * cosSin.x - v.y * cosSin.y

  /** Rotates the vector counterclockwise by `angle` degrees. */
  def rotated(angle: Double): Vector2D =
    val radians = math.toRadians(angle)
    val cosSin  = Vector2D(math.cos(radians), math.sin(radians))
    val sinCos  = cosSin.swap
    Vector2D(
      v rotatedX cosSin,
      v dot sinCos
    )

  /** Alias for [[add]]. */
  def +(toAdd: Vector2D): Vector2D = v add toAdd

  /** Alias for [[sub]]. */
  def -(toSub: Vector2D): Vector2D = v sub toSub

  /** Alias for [[times]]. */
  def *(scalar: Double): Vector2D = v times scalar

  /** Alias for [[squaredDistance]]. */
  def -->>(other: Vector2D): Double = v squaredDistance other

  /** Alias for [[euclideanDistance]]. */
  def -->(other: Vector2D): Double = v euclideanDistance other
