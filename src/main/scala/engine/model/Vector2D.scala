package engine.model

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

  infix def euclideanDistance(other: Vector2D): Double =
    math.sqrt(((v.x - other.x) ** 2) + ((v.y - other.y) ** 2))

  def +(toAdd: Vector2D): Vector2D = v add toAdd

  def *(scalar: Double): Vector2D = v times scalar

  def -->(other: Vector2D): Double = v euclideanDistance other