package monad_core.simulator.infrastructure.ai

import monad_core.engine.model.*
import monad_core.simulator.application.engine.errors.ErrorsAdapter.adaptError
import monad_core.simulator.errors.BaseError

final private[ai] case class EntityOptionalFields(
    teamId: String = null,
    weight: Integer = null,
    speedX: java.lang.Double = null,
    speedY: java.lang.Double = null,
    angularSpeed: java.lang.Double = null,
    health: Integer = null,
    damage: Integer = null
)

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

  private def withOptionalEngineField[A, B](
      current: A,
      value: Option[B]
  )(
      update: (A, B) => Either[EngineError, A]
  ): Either[BaseError, A] =
    value
      .fold(Right(current): Either[EngineError, A])(value => update(current, value))
      .adaptError()

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
