package monad_core.simulator.infrastructure.ai

import monad_core.engine.model.*
import monad_core.simulator.application.engine.errors.ErrorsAdapter.adaptError
import monad_core.simulator.errors.BaseError

/**
 * Record class representing the entity optional fields
 * @param teamId team id
 * @param weight the entity weight
 * @param speedX x coordinate of the entity speed
 * @param speedY y coordinate of the entity speed
 * @param angularSpeed angular speed - velocity by which the entity spins
 * @param health the entity health
 * @param damage the entity damage
 */
final private[ai] case class EntityOptionalFields(
    teamId: String = null,
    weight: Integer = null,
    speedX: java.lang.Double = null,
    speedY: java.lang.Double = null,
    angularSpeed: java.lang.Double = null,
    health: Integer = null,
    damage: Integer = null
)

/**
 * Record class representing the surface optional fields
 *
 * @param frictionIndex friction index - any entity in the surface is slowed by an index
 * @param appliedForceX x coordinate of the vector which is applied to any entity in the surface - representing a force
 * @param appliedForceY y coordinate of the vector which is applied to any entity in the surface - representing a force
 * @param damageOverTime damage dealt by this surface to any entity inside of it
 */
final private[ai] case class SurfaceOptionalFields(
    frictionIndex: java.lang.Double = null,
    appliedForceX: java.lang.Double = null,
    appliedForceY: java.lang.Double = null,
    damageOverTime: Integer = null
)

final private[ai] case class IncompleteEntitySpeed()
    extends BaseError("Both speedX and speedY must be provided together")

final private[ai] case class IncompleteSurfaceAppliedForce()
    extends BaseError("Both appliedForceX and appliedForceY must be provided together")

private[ai] object Langchain4jToolOptions:

  /**
   * Applies the optional fields supplied by the AI tool layer to an entity.
   *
   * A `null` field is treated as absent and leaves the corresponding entity property unchanged. Speed is applied only
   * when both coordinates are provided; supplying a single coordinate produces an [[IncompleteEntitySpeed]]. All
   * engine validation errors raised while applying the remaining fields are adapted to [[BaseError]], and the first
   * error stops the update chain.
   *
   * @see [[EntityOptionalFields]], [[withOptionalEngineField]] and [[optionalVector]]
   * @param entity the entity to enrich with the provided optional values
   * @param fields the optional values received from the AI tool layer
   * @return `Left(BaseError)` when a provided value is invalid or the speed vector is incomplete,
   *
   *         `Right(Entity)` containing all valid provided values and preserving every absent field otherwise
   */
  def applyTo(entity: Entity, fields: EntityOptionalFields): Either[BaseError, Entity] =
    for
      entityWithTeam <- withOptionalEngineField(entity, Option(fields.teamId))((entity, teamId) =>
        entity.withTeamId(teamId)
      )

      entityWithWeight <- withOptionalEngineField(
        entityWithTeam,
        Option(fields.weight).map(_.intValue())
      )((entity, weight) => entity.withWeight(weight))

      entityWithHealth <- withOptionalEngineField(
        entityWithWeight,
        Option(fields.health).map(_.intValue())
      )((entity, health) => entity.withHealth(health))

      entityWithDamage <- withOptionalEngineField(
        entityWithHealth,
        Option(fields.damage).map(_.intValue())
      )((entity, damage) => entity.withDamage(damage))

      entityWithSpeed <- optionalVector(
        fields.speedX,
        fields.speedY,
        IncompleteEntitySpeed()
      ).map(_.fold(entityWithDamage)(entityWithDamage.withSpeed))
    yield Option(fields.angularSpeed).fold(entityWithSpeed)(value =>
      entityWithSpeed.withAngularSpeed(value.doubleValue())
    )

  /**
   * Applies the optional fields supplied by the AI tool layer to a surface.
   *
   * A `null` field is treated as absent and leaves the corresponding surface property unchanged. Applied force is
   * updated atomically: both coordinates must be provided, while a single coordinate produces an
   * [[IncompleteSurfaceAppliedForce]]. Each engine validation error is adapted to [[BaseError]], and the first error
   * stops the update chain.
   *
   * @see [[SurfaceOptionalFields]], [[withOptionalEngineField]] and [[optionalVector]]
   * @param surface the surface to enrich with the provided optional values
   * @param fields the optional values received from the AI tool layer
   * @return `Left(BaseError)` when a provided value is invalid or the applied-force vector is incomplete,
   *
   *         `Right(Surface)` containing all valid provided values and preserving every absent field otherwise
   */
  def applyTo(surface: Surface, fields: SurfaceOptionalFields): Either[BaseError, Surface] =
    for
      surfaceWithFriction <- withOptionalEngineField(
        surface,
        Option(fields.frictionIndex).map(_.doubleValue())
      )((surface, frictionIndex) => surface.withFrictionIndex(frictionIndex))
      surfaceWithForce <- optionalVector(
        fields.appliedForceX,
        fields.appliedForceY,
        IncompleteSurfaceAppliedForce()
      ).flatMap:
        case None        => Right(surfaceWithFriction)
        case Some(force) => surfaceWithFriction.withAppliedForce(force).adaptError()
      completeSurface <- withOptionalEngineField(
        surfaceWithForce,
        Option(fields.damageOverTime).map(_.intValue())
      )((surface, damageOverTime) => surface.withDamageOverTime(damageOverTime))
    yield completeSurface

  /**
   * Applies an optional value through an engine update function.
   *
   * When the value is absent, the current instance is returned unchanged and the update function is not evaluated.
   * When it is present, the update is executed and any [[EngineError]] is adapted to [[BaseError]].
   *
   * @tparam A the type of the instance being updated
   * @tparam B the type of the optional value
   * @param current the instance to return unchanged or pass to the update function
   * @param value the value to apply, or `None` when the field was not provided
   * @param update the engine update function to execute for a provided value
   * @return `Left(BaseError)` when the engine rejects the update,
   *
   *         `Right(A)` containing either the unchanged instance or its updated copy otherwise
   */
  private def withOptionalEngineField[A, B](
      current: A,
      value: Option[B]
  )(
      update: (A, B) => Either[EngineError, A]
  ): Either[BaseError, A] =
    value
      .fold(Right(current): Either[EngineError, A])(value => update(current, value))
      .adaptError()

  /**
   * Converts two nullable coordinates into an optional engine vector while enforcing their mutual presence.
   *
   * Two absent coordinates represent an omitted vector and yield `None`; two provided coordinates yield a
   * [[Vector2D]]. If exactly one coordinate is present, the supplied by-name error is evaluated and returned.
   *
   * @param x the nullable horizontal coordinate
   * @param y the nullable vertical coordinate
   * @param incompleteError the error to produce lazily when only one coordinate is provided
   * @return `Left(BaseError)` when the coordinate pair is incomplete,
   *
   *         `Right(None)` when both coordinates are absent, or `Right(Some(Vector2D))` when both are present
   */
  private def optionalVector(
      x: java.lang.Double,
      y: java.lang.Double,
      incompleteError: => BaseError
  ): Either[BaseError, Option[Vector2D]] =
    (Option(x), Option(y)) match
      case (None, None) => Right(None)
      case (Some(horizontal), Some(vertical)) =>
        Right(Some(Vector2D(horizontal.doubleValue(), vertical.doubleValue())))
      case _ => Left(incompleteError)
