package monad_core.engine.physics.utils

import monad_core.engine.model.*
import monad_core.engine.physics.core.{NegativeDeltaTime, PhysicsError, ZeroMassError}

/** Pure calculations shared by physics rules and collision resolution. */
private[physics] object PhysicsUtil:

  private val NanosecondsPerSecond = 1_000_000_000.0
  private val VectorZero: Vector2D = Vector2D(0.0, 0.0)

  /** Converts a mass to its inverse value. */
  val inverseMass: Double => Double = mass => 1.0 / mass

  /**
   * Converts a non-negative nanosecond duration to seconds.
   *
   * @param deltaTime
   *   duration in nanoseconds
   * @return
   *   duration in seconds, or a [[NegativeDeltaTime]]
   */
  def timeLongToSeconds(deltaTime: Long): Either[PhysicsError, Double] =
    deltaTime match
      case t if t < 0L => Left(NegativeDeltaTime(deltaTime))
      case t           => Right(t.toDouble / NanosecondsPerSecond)

  /**
   * Calculates linear displacement with constant velocity.
   *
   * @param speed
   *   linear velocity in world units per second
   * @param deltaTime
   *   elapsed nanoseconds
   * @return
   *   displacement vector, or a [[PhysicsError]]
   */
  def displacement(speed: Vector2D, deltaTime: Long): Either[PhysicsError, Vector2D] =
    for seconds <- timeLongToSeconds(deltaTime)
    yield speed * seconds

  /**
   * Calculates the next position reached with constant linear velocity.
   *
   * @param position
   *   initial position
   * @param speed
   *   linear velocity
   * @param deltaTime
   *   elapsed nanoseconds
   * @return
   *   next position, or a [[PhysicsError]]
   */
  def nextPosition(
      position: Vector2D,
      speed: Vector2D,
      deltaTime: Long
  ): Either[PhysicsError, Vector2D] =
    for disp <- displacement(speed, deltaTime)
    yield position + disp

  /**
   * Calculates acceleration according to `force / mass`.
   *
   * @param force
   *   applied force vector
   * @param mass
   *   optional entity weight
   * @return
   *   acceleration vector, or a [[ZeroMassError]]
   */
  def acceleration(
      force: Vector2D,
      mass: Option[Weight]
  ): Either[PhysicsError, Vector2D] =
    for actualMass <- actualDoubleWeight(mass)
    yield force * inverseMass(actualMass)

  /**
   * Applies linear friction over the supplied duration.
   *
   * @param speed
   *   current linear velocity
   * @param frictionIndex
   *   velocity reduction per second
   * @param deltaTime
   *   elapsed nanoseconds
   * @return
   *   reduced velocity, or a [[PhysicsError]]
   */
  def applyFriction(
      speed: Vector2D,
      frictionIndex: Double,
      deltaTime: Long
  ): Either[PhysicsError, Vector2D] =
    frictionFactor(frictionIndex, deltaTime).map(speed * _)

  /**
   * Applies angular friction over the supplied duration.
   *
   * @param angularSpeed
   *   current angular velocity
   * @param frictionIndex
   *   velocity reduction per second
   * @param deltaTime
   *   elapsed nanoseconds
   * @return
   *   reduced angular velocity, or a [[PhysicsError]]
   */
  def applyAngularFriction(
      angularSpeed: Double,
      frictionIndex: Double,
      deltaTime: Long
  ): Either[PhysicsError, Double] =
    frictionFactor(frictionIndex, deltaTime).map(angularSpeed * _)

  /**
   * Calculates the friction multiplier for a given duration.
   *
   * @param frictionIndex
   *   velocity reduction per second
   * @param deltaTime
   *   elapsed nanoseconds
   * @return
   *   clamped multiplier, or a [[PhysicsError]]
   */
  private def frictionFactor(
      frictionIndex: Double,
      deltaTime: Long
  ): Either[PhysicsError, Double] =
    for seconds <- timeLongToSeconds(deltaTime)
    yield math.max(0.0, 1.0 - frictionIndex * seconds)

  /**
   * Projects velocity onto a collision normal and retains only incoming motion.
   *
   * @param speed
   *   relative velocity
   * @param normal
   *   collision unit normal
   * @return
   *   non-positive incoming normal velocity
   */
  def incomingSpeedAlongNormal(
      speed: Vector2D,
      normal: Vector2D
  ): Double =
    math.min(speed dot normal, 0.0)

  /**
   * Calculates the scalar impulse for a perfectly elastic collision.
   *
   * @param incomingSpeedAlongNormal
   *   non-positive relative velocity along the normal
   * @param totalInverseMass
   *   effective inverse mass of both bodies
   * @return
   *   scalar impulse, or zero when no body can move
   */
  def computeImpulse(
      incomingSpeedAlongNormal: Double,
      totalInverseMass: Double
  ): Double =
    if totalInverseMass == 0.0 then 0.0
    else -2.0 * incomingSpeedAlongNormal / totalInverseMass

  /**
   * Reflects a velocity against a fixed body using the collision normal.
   *
   * @param speed
   *   incoming velocity
   * @param normal
   *   collision unit normal
   * @return
   *   reflected velocity
   */
  def reflectOnFixed(
      speed: Vector2D,
      normal: Vector2D
  ): Vector2D =
    val twiceIncomingSpeedAlongNormal = 2.0 * incomingSpeedAlongNormal(speed, normal)

    speed - (normal * twiceIncomingSpeedAlongNormal)

  /**
   * Pushes a mobile body outside a fixed overlapping body.
   *
   * @param position
   *   current mobile-body position
   * @param normal
   *   collision unit normal
   * @param penetrationDepth
   *   overlap distance
   * @return
   *   corrected position
   */
  def pushMobileOverlappingFixed(
      position: Vector2D,
      normal: Vector2D,
      penetrationDepth: Double
  ): Vector2D =
    position + (normal * penetrationDepth)

  /**
   * Converts a present weight to `Double`.
   *
   * @param weight
   *   optional entity weight
   * @return
   *   numeric mass, or a [[ZeroMassError]]
   */
  def actualDoubleWeight(weight: Option[Weight]): Either[PhysicsError, Double] =
    weight match
      case None =>
        Left(ZeroMassError())
      case Some(w) =>
        Right(w.value.toDouble)

  /**
   * Calculates the elastic response velocity of one mobile body against another.
   *
   * @param speed
   *   first-body velocity
   * @param otherSpeed
   *   second-body velocity
   * @param normal
   *   collision unit normal
   * @param mass
   *   first-body weight
   * @param massOther
   *   second-body weight
   * @return
   *   first-body response velocity, or a [[ZeroMassError]]
   */
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

  /**
   * Distributes an overlap correction between two mobile bodies according to their masses.
   * The lighter contribution moves farther because correction follows the other body's mass ratio.
   *
   * @param position
   *   first-body position
   * @param normal
   *   separation unit normal
   * @param penetrationDepth
   *   overlap distance
   * @param mass
   *   first-body weight
   * @param massOther
   *   second-body weight
   * @return
   *   corrected first-body position, or a [[ZeroMassError]]
   */
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

  /**
   * Finds the nearest entity belonging to an enemy team.
   *
   * @param entity
   *   entity searching for an enemy
   * @param entities
   *   candidate entities
   * @param teams
   *   team relationships
   * @return
   *   nearest enemy, or `None` when team data or enemies are absent
   */
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
