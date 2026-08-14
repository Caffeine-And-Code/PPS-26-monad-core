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

  def +(toAdd: Vector2D): Vector2D = v add toAdd

  def -(toSub: Vector2D): Vector2D = v sub toSub

  def *(scalar: Double): Vector2D = v times scalar

  def o(toDot: Vector2D): Double = v dot toDot

  def -->(other: Vector2D): Double = v euclideanDistance other