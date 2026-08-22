package monad_core.engine.physics.utils

import monad_core.engine.geometry.Collision
import monad_core.engine.model.*
import monad_core.engine.physics.core.{NegativeDeltaTime, PhysicsError, ZeroMassError}

private[physics] object PhysicsUtil:

  private val NanosecondsPerSecond = 1_000_000_000.0
  private val VectorZero: Vector2D = Vector2D(0.0, 0.0)

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
    yield force * (1.0 / actualMass)

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

  def reflectOnFixed(
      speed: Vector2D,
      normal: Vector2D
  ): Vector2D =
    val speedAlongNormal              = speed dot normal
    val incomingSpeedAlongNormal      = math.min(speedAlongNormal, 0.0)
    val twiceIncomingSpeedAlongNormal = 2.0 * incomingSpeedAlongNormal

    speed - (normal * twiceIncomingSpeedAlongNormal)

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
      actualMass      <- actualDoubleWeight(mass)
      actualOtherMass <- actualDoubleWeight(massOther)

      relativeVelocity = speed - otherSpeed

      velocityAlongNormal         = relativeVelocity dot normal
      incomingVelocityAlongNormal = math.min(velocityAlongNormal, 0.0)

      impulse =
        -2.0 * incomingVelocityAlongNormal / (1.0 / actualMass + 1.0 / actualOtherMass)

      actualSpeed = speed + (normal * (impulse / actualMass))
    yield actualSpeed

  def speedAtPoint(entity: Entity, point: Vector2D): Vector2D =
    val linearSpeed = entity.speed.getOrElse(VectorZero)
    val rotationSpeed = entity.angularSpeed
      .map: angularSpeed =>
        val radius = point - entity.position
        Vector2D(-radius.y, radius.x) * math.toRadians(angularSpeed)
      .getOrElse(VectorZero)

    linearSpeed + rotationSpeed

  def collisionResponse(
      entity: Entity,
      other: Entity,
      collision: Collision
  ): Either[PhysicsError, (Vector2D, Double)] =
    for
      mass <- actualDoubleWeight(entity.weight)
      otherMass = other.weight.map(_.value.toDouble).getOrElse(1.0)
      relativeSpeed =
        speedAtPoint(entity, collision.collisionPoint) -
          speedAtPoint(other, collision.collisionPoint)
      incomingSpeed = math.min(relativeSpeed dot collision.normalVector, 0.0)
      radius        = collision.collisionPoint - entity.position
      otherRadius   = collision.collisionPoint - other.position
      inertia       = momentOfInertia(entity.shape, mass)
      otherInertia  = momentOfInertia(other.shape, otherMass)
      inverseMass =
        (if entity.speed.isDefined then 1.0 / mass else 0.0) +
          (if other.speed.isDefined then 1.0 / otherMass else 0.0) +
          (if entity.angularSpeed.isDefined then
             math.pow(radius cross collision.normalVector, 2.0) / inertia
           else 0.0) +
          (if other.angularSpeed.isDefined then
             math.pow(otherRadius cross collision.normalVector, 2.0) / otherInertia
           else 0.0)
      impulse       = if inverseMass == 0.0 then 0.0 else -2.0 * incomingSpeed / inverseMass
      impulseVector = collision.normalVector * impulse
      speedChange =
        if entity.speed.isDefined then impulseVector * (1.0 / mass)
        else VectorZero
      angularSpeedChange =
        if entity.angularSpeed.isDefined then math.toDegrees((radius cross impulseVector) / inertia)
        else 0.0
    yield speedChange -> angularSpeedChange

  private def momentOfInertia(shape: Shape2D, mass: Double): Double =
    shape match
      case Shape2D.Circle(radius) => mass * radius * radius / 2.0
      case Shape2D.Rectangle(height, length) =>
        mass * (height * height + length * length) / 12.0

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
