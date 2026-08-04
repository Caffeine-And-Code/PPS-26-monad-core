package monad_core.engine.model

import scala.math

case class Vector2D(x: Double, y: Double)

extension (value: Double)
  def **(other: Double): Double =
    math.pow(value, other)

extension (v: Vector2D)
  infix def add(toAdd: Vector2D): Vector2D =
    Vector2D(v.x + toAdd.x, v.y + toAdd.y)

  infix def times(scalar: Double): Vector2D =
    Vector2D(v.x * scalar, v.y * scalar)

  infix def subtract(toSubtract: Vector2D): Vector2D =
    Vector2D(v.x - toSubtract.x, v.y - toSubtract.y)

  infix def euclideanDistance(other: Vector2D): Double =
    math.sqrt(((v.x - other.x) ** 2) + ((v.y - other.y) ** 2))

  def magnitude: Double =
    math.sqrt((v.x ** 2) + (v.y ** 2))

  def normalized: Vector2D =
    val currentMagnitude = v.magnitude

    if currentMagnitude == 0 then
      Vector2D(0, 0)
    else
      Vector2D(v.x / currentMagnitude, v.y / currentMagnitude)

  def flip: Vector2D =
    v.copy(-v.x, -v.y)

  infix def dot(other: Vector2D): Double =
    v.x * other.x + v.y * other.y

  def rotated(angle: Double): Vector2D =
    val radians = math.toRadians(angle)
    Vector2D(
      v.x * math.cos(radians) - v.y * math.sin(radians),
      v.x * math.sin(radians) + v.y * math.cos(radians)
    )

  def +(toAdd: Vector2D): Vector2D = v add toAdd

  def -(toSubtract: Vector2D): Vector2D = v subtract toSubtract

  def *(scalar: Double): Vector2D = v times scalar

  def -->(other: Vector2D): Double = v euclideanDistance other
