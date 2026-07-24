package engine.physics.core

import engine.errors.EngineError
import engine.model.*

object PhysicsUtil:

  private val NanosecondsPerSecond = 1_000_000_000.0
  private val VectorZero: Vector2D = Vector2D(0.0, 0.0)
  private val LongZero: Long = 0L
  private val DoubleZero: Double = 0.0
  private val Numerator = 1.0

  def deltaSeconds(deltaTime: Long): Either[PhysicsError, Double] =
    deltaTime match
      case t if t < LongZero => Left(NegativeDeltaTime(deltaTime))
      case t           => Right(t.toDouble / NanosecondsPerSecond)

  def displacement(speed: Vector2D, deltaTime: Long): Either[PhysicsError, Vector2D] =
    for
      seconds <- deltaSeconds(deltaTime)
    yield speed * seconds

  def nextPosition(
                    position: Vector2D,
                    speed: Vector2D,
                    deltaTime: Long
                  ): Either[PhysicsError, Vector2D] =
    displacement(speed, deltaTime).map(position + _)

  def acceleration(
                    force: Vector2D,
                    mass: Either[EngineError, Weight]
                  ): Either[EngineError, Vector2D] =
    mass.map(m => force * (Numerator / m.value.toDouble))

  def nextSpeed(
                 speed: Vector2D,
                 acceleration: Vector2D,
                 deltaTime: Long
               ): Either[PhysicsError, Vector2D] =
    displacement(acceleration, deltaTime).map(speed + _)

  def applyFriction(
                     speed: Vector2D,
                     frictionIndex: Double,
                     deltaTime: Long
                   ): Either[PhysicsError, Vector2D] =
    deltaSeconds(deltaTime).map(seconds =>
      val factor = math.max(DoubleZero, Numerator - frictionIndex * seconds)
      speed * factor
    )

  def squaredDistance(first: Vector2D, second: Vector2D): Double =
    val dx = second.x - first.x
    val dy = second.y - first.y
    dx * dx + dy * dy

  def direction(
                 from: Vector2D,
                 to: Vector2D
               ): Option[Vector2D] =
    val delta = Vector2D(to.x - from.x, to.y - from.y)
    val squaredLength = squaredDistance(from, to)

    Option.when(squaredLength > DoubleZero):
      delta * (Numerator / math.sqrt(squaredLength))