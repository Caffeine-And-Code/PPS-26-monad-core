package monad_core.engine.model

import monad_core.engine.errors.EngineError

final case class Entity private(
                                 id: LocatableId,
                                 position: Vector2D,
                                 shape: Shape2D,
                                 rotation: Double,
                                 speed: Option[Vector2D] = None,
                                 angularSpeed: Option[Double] = None,
                                 weight: Option[Weight] = None,
                                 health: Option[Health] = None,
                                 teamId: Option[TeamId] = None
                               ) extends Locatable

object Entity:
  def circle(id: String, position: Vector2D, radius: Double, rotation: Double = 0): Either[EngineError, Entity] =
    Locatable.circle(id, position, radius, rotation)((id, position, shape, rotation) => Entity(id, position, shape, rotation))

  def rectangle(id: String, position: Vector2D, height: Double, length: Double, rotation: Double = 0): Either[EngineError, Entity] =
    Locatable.rectangle(id, position, height, length, rotation)((id, position, shape, rotation) => Entity(id, position, shape, rotation))

  private def validateAndReturn(updated: Entity): Either[EngineError, Entity] =
    Entity.validate(updated).map(_ => updated)

  def validate(entity: Entity): Either[EngineError, Unit] =
    for {
      result <- Locatable.validatePosition(entity.position)
      _ <- Locatable.validateRotation(entity.rotation)
    } yield result

  extension (entity: Entity)

    def moveTo(newPosition: Vector2D): Either[EngineError, Entity] =
      validateAndReturn(entity.copy(position = newPosition))

    def moveBy(space: Vector2D): Either[EngineError, Entity] =
      validateAndReturn(entity.copy(position = entity.position + space))

    def rotateTo(rotation: Double): Either[EngineError, Entity] =
      validateAndReturn(entity.copy(rotation = rotation))

    def withSpeed(speed: Vector2D): Either[EngineError, Entity] =
      validateAndReturn(entity.copy(speed = Some(speed)))

    def withoutSpeed: Entity =
      entity.copy(speed = None)

    def withAngularSpeed(angularSpeed: Double): Entity =
      entity.copy(angularSpeed = Some(angularSpeed))

    def withoutAngularSpeed: Entity =
      entity.copy(angularSpeed = None)

    def isFixed: Boolean =
      entity.speed.isEmpty && entity.angularSpeed.isEmpty

    def withWeight(weight: Int): Either[EngineError, Entity] =
      Weight(weight).map(w => entity.copy(weight = Some(w)))

    def withHealth(health: Int): Either[EngineError, Entity] =
      Health(health).map(h => entity.copy(health = Some(h)))

    def applyDamage(damage: Int): Either[EngineError, Entity] =
      entity.health match
        case None => Left(CannotApplyDamageToNoneHealthEntity())
        case Some(health) => (health - damage).map(health => entity.copy(health = Some(health)))

    def withTeamId(teamId: String): Either[EngineError, Entity] =
      TeamId(teamId).map(t => entity.copy(teamId = Some(t)))
