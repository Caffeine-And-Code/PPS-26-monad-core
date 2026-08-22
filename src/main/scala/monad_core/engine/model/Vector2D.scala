package monad_core.engine.model

import scala.math

case class Vector2D(x: Double, y: Double)

extension (value: Double)

  def **(other: Double): Double =
    math.pow(value, other)

extension (v: Vector2D)

  infix def add(toAdd: Vector2D): Vector2D =
    Vector2D(v.x + toAdd.x, v.y + toAdd.y)

  infix def sub(toSub: Vector2D): Vector2D =
    Vector2D(v.x - toSub.x, v.y - toSub.y)

  infix def times(scalar: Double): Vector2D =
    Vector2D(v.x * scalar, v.y * scalar)

  infix def dot(toDot: Vector2D): Double =
    v.x * toDot.x + v.y * toDot.y

  infix def cross(toCross: Vector2D): Double =
    v.x * toCross.y - v.y * toCross.x

  infix def squaredDistance(other: Vector2D): Double =
    ((v.x - other.x) ** 2) + ((v.y - other.y) ** 2)

  infix def euclideanDistance(other: Vector2D): Double =
    math.sqrt(squaredDistance(other))

  def magnitude: Double =
    math.sqrt((v.x ** 2) + (v.y ** 2))

  def normalized: Vector2D =
    val currentMagnitude = v.magnitude

    if currentMagnitude == 0 then Vector2D(0, 0)
    else Vector2D(v.x / currentMagnitude, v.y / currentMagnitude)

  def flip: Vector2D =
    v.copy(-v.x, -v.y)

  def swap: Vector2D =
    v.copy(v.y, v.x)

  private infix def rotatedX(cosSin: Vector2D): Double =
    v.x * cosSin.x - v.y * cosSin.y

  def rotated(angle: Double): Vector2D =
    val radians = math.toRadians(angle)
    val cosSin  = Vector2D(math.cos(radians), math.sin(radians))
    val sinCos  = cosSin.swap
    Vector2D(
      v rotatedX cosSin,
      v dot sinCos
    )

  def +(toAdd: Vector2D): Vector2D = v add toAdd

  def -(toSub: Vector2D): Vector2D = v sub toSub

  def *(scalar: Double): Vector2D = v times scalar

  def -->>(other: Vector2D): Double = v squaredDistance other

  def -->(other: Vector2D): Double = v euclideanDistance other
