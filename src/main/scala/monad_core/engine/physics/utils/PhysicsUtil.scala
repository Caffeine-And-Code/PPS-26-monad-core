package monad_core.engine.physics.utils

import monad_core.engine.errors.EngineError
import monad_core.engine.model.*
import monad_core.engine.physics.core.{NegativeDeltaTime, OutOfBoundEntity, PhysicsError, ZeroMassError}

private[physics] object PhysicsUtil:

  private val NanosecondsPerSecond = 1_000_000_000.0
  private val VectorZero: Vector2D = Vector2D(0.0, 0.0)

  def deltaSeconds(deltaTime: Long): Either[PhysicsError, Double] =
    deltaTime match
      case t if t < 0L => Left(NegativeDeltaTime(deltaTime))
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
  ): Either[PhysicsError, Vector2D] =
    val actualMass = actualDoubleWeight(mass)
    actualMass match
      case Left(err) => Left(err)
      case Right(m)  => Right(force * (1.0 / m))

  def applyFriction(
    speed: Vector2D,
    frictionIndex: Double,
    deltaTime: Long
  ): Either[PhysicsError, Vector2D] = {
    for
      seconds <- deltaSeconds(deltaTime)
      factor = math.max(0.0, 1.0 - frictionIndex * seconds)
    yield speed * factor
  }

  def squaredDistance(first: Vector2D, second: Vector2D): Double =
    val dx = second.x - first.x
    val dy = second.y - first.y
    dx * dx + dy * dy
    
  def distance(first: Vector2D, second: Vector2D): Double =
    math.sqrt(squaredDistance(first, second))

  def reflectOnFixed(
    speed: Vector2D,
    normal: Vector2D
  ): Vector2D =
    val speedAlongNormal = speed dot normal

    if speedAlongNormal >= 0.0 then
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

    for
      actualMass <- actualDoubleWeight(mass)
      actualOtherMass <- actualDoubleWeight(massOther)

      relativeVelocity = speed - otherSpeed

      velocityAlongNormal = relativeVelocity dot normal

      impulse = -2.0 * velocityAlongNormal / (1.0 / actualMass + 1.0 / actualOtherMass)

      actualSpeed = speed + (normal * (impulse / actualMass))

    yield actualSpeed

  def pushMobileOverlappingMobile(
    position: Vector2D,
    normal: Vector2D,
    penetrationDepth: Double,
    mass: Option[Weight],
    massOther: Option[Weight]
  ): Either[PhysicsError, Vector2D] =
    for
      actualMass <- actualDoubleWeight(mass)
      actualOtherMass <- actualDoubleWeight(massOther)

      totalWeight = actualMass + actualOtherMass
      ratio = actualMass / totalWeight

      correction = normal * (penetrationDepth * ratio)

      newPosition = position + correction
    yield newPosition

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