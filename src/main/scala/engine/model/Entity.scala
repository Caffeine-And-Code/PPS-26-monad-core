package engine.model

final case class Entity private(
                                 id: LocatableId,
                                 position: Vector2D,
                                 shape: Shape2D,
                                 speed: Option[Vector2D] = None,
                                 weight: Option[Weight] = None,
                                 health: Option[Health] = None,
                                 teamId: Option[TeamId] = None
                               ) extends Locatable

object Entity:
  def circle(id: String, position: Vector2D, radius: Double): Either[String, Entity] =
    Locatable.circle(id, position, radius)((id, position, shape) => Entity(id, position, shape))

  def rectangle(id: String, position: Vector2D, height: Double, length: Double): Either[String, Entity] =
    Locatable.rectangle(id, position, height, length)((id, position, shape) => Entity(id, position, shape))

  private def validateAndReturn(updated: Entity): Either[String, Entity] =
    Entity.validate(updated).map(_ => updated)

  def validate(entity: Entity): Either[String, Unit] =
    for {
      result <- Locatable.validate(entity.position)
    } yield result

  extension (entity: Entity)

    def moveTo(newPosition: Vector2D): Either[String, Entity] =
      validateAndReturn(entity.copy(position = newPosition))

    def moveBy(space: Vector2D): Either[String, Entity] =
      validateAndReturn(entity.copy(position = entity.position + space))

    def withSpeed(speed: Vector2D): Either[String, Entity] =
      validateAndReturn(entity.copy(speed = Some(speed)))

    def withoutSpeed: Entity =
      entity.copy(speed = None)

    def isFixed: Boolean =
      entity.speed.isEmpty

    def withWeight(weight: Int): Either[String, Entity] =
      Weight(weight).map(w => entity.copy(weight = Some(w)))

    def withHealth(health: Int): Either[String, Entity] =
      Health(health).map(h => entity.copy(health = Some(h)))

    def applyDamage(damage: Int): Either[String, Entity] =
      entity.health match
        case None => Left("Cannot apply damage to None health entity")
        case Some(health) => (health - damage).map(health => entity.copy(health = Some(health)))

    def withTeamId(teamId: String): Either[String, Entity] =
      TeamId(teamId).map(t => entity.copy(teamId = Some(t)))