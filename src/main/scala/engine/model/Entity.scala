package engine.model

final case class Entity(
                            id: String,
                            position: Vector2D,
                            shape: Shape2D,
                            speed: Option[Vector2D] = None,
                            weight: Option[Weight]  = None,
                            health: Option[Health]  = None
                          ) extends Locatable

object Entity:
  def circle(id: String, position: Vector2D, radius: Double):Either[String, Entity] =
    Locatable.circle(id, position, radius)((id, position, shape) => Entity(id, position, shape))

  def rectangle(id: String, position: Vector2D, height: Double, length: Double):Either[String, Entity] =
    Locatable.rectangle(id, position, height, length)((id, position, shape) => Entity(id, position, shape))

  def validate(entity: Entity): Either[String, Unit] =
    for {
      result <- Locatable.validate(entity.id, entity.position)
    } yield result

  private def validateAndReturn(updated: Entity): Either[String, Entity] =
    Entity.validate(updated).map(_ => updated)

  extension (e: Entity)

    def moveTo(newPosition: Vector2D): Either[String, Entity] =
      validateAndReturn(e.copy(position = newPosition))

    def moveBy(space: Vector2D): Either[String, Entity] =
      validateAndReturn(e.copy(position = e.position + space))

    def withSpeed(speed: Vector2D): Either[String, Entity] =
      validateAndReturn(e.copy(speed = Some(speed)))

    def withWeight(weight: Int): Either[String, Entity] =
      Weight(weight).map(w => e.copy(weight = Some(w)))

    def withHealth(health: Int): Either[String, Entity] =
      Health(health).map(h => e.copy(health = Some(h)))