package monad_core.engine.physics.utils

import monad_core.engine.model.*
import monad_core.engine.physics.core.{NegativeDeltaTime, PhysicsError, ZeroMassError}

private[physics] object PhysicsUtil:

  private val NanosecondsPerSecond = 1_000_000_000.0
  private val VectorZero: Vector2D = Vector2D(0.0, 0.0)

  val inverseMass: Double => Double = mass => 1.0 / mass

  def timeLongToSeconds(deltaTime: Long): Either[PhysicsError, Double] =
    deltaTime match
      case t if t < 0L => Left(NegativeDeltaTime(deltaTime))
      case t           => Right(t.toDouble / NanosecondsPerSecond)

  def displacement(speed: Vector2D, deltaTime: Long): Either[PhysicsError, Vector2D] =
    for seconds <- timeLongToSeconds(deltaTime)
    yield speed * seconds

  def nextPosition(
      position: Vector2D,
      speed: Vector2D,
      deltaTime: Long
  ): Either[PhysicsError, Vector2D] =
    for disp <- displacement(speed, deltaTime)
    yield position + disp

  def acceleration(
      force: Vector2D,
      mass: Option[Weight]
  ): Either[PhysicsError, Vector2D] =
    for actualMass <- actualDoubleWeight(mass)
    yield force * inverseMass(actualMass)

  def applyFriction(
      speed: Vector2D,
      frictionIndex: Double,
      deltaTime: Long
  ): Either[PhysicsError, Vector2D] =
    frictionFactor(frictionIndex, deltaTime).map(speed * _)

  def applyAngularFriction(
      angularSpeed: Double,
      frictionIndex: Double,
      deltaTime: Long
  ): Either[PhysicsError, Double] =
    frictionFactor(frictionIndex, deltaTime).map(angularSpeed * _)

  private def frictionFactor(
      frictionIndex: Double,
      deltaTime: Long
  ): Either[PhysicsError, Double] =
    for seconds <- timeLongToSeconds(deltaTime)
    yield math.max(0.0, 1.0 - frictionIndex * seconds)

  def incomingSpeedAlongNormal(
      speed: Vector2D,
      normal: Vector2D
  ): Double =
    math.min(speed dot normal, 0.0)

  def computeImpulse(
      incomingSpeedAlongNormal: Double,
      totalInverseMass: Double
  ): Double =
    if totalInverseMass == 0.0 then 0.0
    else -2.0 * incomingSpeedAlongNormal / totalInverseMass

  def reflectOnFixed(
      speed: Vector2D,
      normal: Vector2D
  ): Vector2D =
    val twiceIncomingSpeedAlongNormal = 2.0 * incomingSpeedAlongNormal(speed, normal)

    speed - (normal * twiceIncomingSpeedAlongNormal)

  def pushMobileOverlappingFixed(
      position: Vector2D,
      normal: Vector2D,
      penetrationDepth: Double
  ): Vector2D =
    position + (normal * penetrationDepth)

  def actualDoubleWeight(weight: Option[Weight]): Either[PhysicsError, Double] =
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
      actualMass      <- actualDoubleWeight(mass)
      actualOtherMass <- actualDoubleWeight(massOther)
    yield
      val relativeVelocity = speed - otherSpeed
      val totalInverseMass = inverseMass(actualMass) + inverseMass(actualOtherMass)
      val incomingSpeed    = incomingSpeedAlongNormal(relativeVelocity, normal)
      val impulse = computeImpulse(
        incomingSpeed,
        totalInverseMass
      )

      speed + (normal * (impulse * inverseMass(actualMass)))

  def pushMobileOverlappingMobile(
      position: Vector2D,
      normal: Vector2D,
      penetrationDepth: Double,
      mass: Option[Weight],
      massOther: Option[Weight]
  ): Either[PhysicsError, Vector2D] =
    for
      actualMass      <- actualDoubleWeight(mass)
      actualOtherMass <- actualDoubleWeight(massOther)

      totalWeight = actualMass + actualOtherMass
      ratio       = actualOtherMass / totalWeight

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
      team   <- teams.find(_.id == teamId)

      enemy <- entities.iterator
        .filter(candidate => candidate.teamId.exists(team.enemies.contains))
        .minByOption(candidate => entity.position -->> candidate.position)
    yield enemy
