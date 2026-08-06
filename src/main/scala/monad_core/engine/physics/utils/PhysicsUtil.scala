package monad_core.engine.physics.utils

import monad_core.engine.errors.EngineError
import monad_core.engine.model.*
import monad_core.engine.physics.core.{NegativeDeltaTime, OutOfBoundEntity, PhysicsError, ZeroMassError}

private[physics] object PhysicsUtil:

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
    deltaTime: Long,
    upperLeftCorner: Vector2D,
    lowerRightCorner: Vector2D
  ): Either[PhysicsError, Vector2D] = {
    for
      disp <- displacement(speed, deltaTime)
      calcPos = position + disp
      nextPos <-
        if calcPos.x < upperLeftCorner.x || calcPos.y < upperLeftCorner.y ||
           calcPos.x > lowerRightCorner.x || calcPos.y > lowerRightCorner.y then
          Left(OutOfBoundEntity(calcPos))
        else
          Right(calcPos)
    yield nextPos
  }

  def acceleration(
    force: Vector2D,
    mass: Option[Weight]
  ): Either[EngineError, Vector2D] =
    val actualMass = actualDoubleWeight(mass)
    actualMass match
      case Left(err) => Left(err)
      case Right(m)  => Right(force * (Numerator / m))

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
    
  def distance(first: Vector2D, second: Vector2D): Double =
    math.sqrt(squaredDistance(first, second))

  def direction(
    from: Vector2D,
    to: Vector2D
  ): Option[Vector2D] =
    val delta = Vector2D(to.x - from.x, to.y - from.y)
    val squaredLength = squaredDistance(from, to)

    Option.when(squaredLength > DoubleZero):
      delta * (Numerator / math.sqrt(squaredLength))

  def reflectOnFixed(
    speed: Vector2D,
    normal: Vector2D
  ): Vector2D =
    val speedAlongNormal = speed dot normal

    if speedAlongNormal >= DoubleZero then
      speed
    else
      speed - (normal * (2.0 * speedAlongNormal))

  def pushMobileOverlappingFixed(
               position: Vector2D,
               normal: Vector2D,
               penetrationDepth: Double
             ): Vector2D =
    position + (normal * penetrationDepth)

  private def actualDoubleWeight(weight: Option[Weight]): Either[PhysicsError, Double] =
    weight match
      case None =>
        Left(ZeroMassError())

      case Some(w) =>
        Right(w.value.toDouble)

  def reflectOnMobile(
     speed: Vector2D,
     otherSpeed: Vector2D,
     normal: Vector2D,
     mass: Option[Weight],
     massOther: Option[Weight]
  ): Either[PhysicsError, Vector2D] =
    val actualMass = actualDoubleWeight(mass)
    val actualOtherMass = actualDoubleWeight(massOther)

    (actualMass, actualOtherMass) match
      case (Left(err), _) => Left(err)
      case (_, Left(err)) => Left(err)
      case (Right(actualMass), Right(actualOtherMass)) =>
        val relativeVelocity = speed - otherSpeed

        val velocityAlongNormal = relativeVelocity dot normal

        if velocityAlongNormal >= DoubleZero then
          Right(speed)
        else
          val impulse =
            -2.0 * velocityAlongNormal / (Numerator / actualMass + Numerator / actualOtherMass)

          Right(speed + (normal * (impulse / actualMass)))

  def pushMobileOverlappingMobile(
    position: Vector2D,
    normal: Vector2D,
    penetrationDepth: Double,
    mass: Option[Weight],
    massOther: Option[Weight]
  ): Either[PhysicsError, Vector2D] =

    val actualMass = actualDoubleWeight(mass)
    val actualOtherMass = actualDoubleWeight(massOther)

    (actualMass, actualOtherMass) match
      case (Left(err), _) => Left(err)
      case (_, Left(err)) => Left(err)
      case (Right(actualMass), Right(actualOtherMass)) =>
        val totalWeight = actualMass + actualOtherMass
        val ratio = actualMass / totalWeight

        val correction = normal * (penetrationDepth * ratio)

        Right(position + correction)

  def nearestEnemy(
    entity: Entity,
    entities: List[Entity],
    teams: List[Team]
  ): Option[Entity] =
    for
      teamId <- entity.teamId
      team <- teams.find(_.id == teamId)

      enemy <- entities.iterator
        .filter(candidate =>
          candidate.teamId.exists(team.enemies.contains)
        )
        .minByOption(candidate =>
          PhysicsUtil.squaredDistance(
            entity.position,
            candidate.position
          )
        )
    yield enemy