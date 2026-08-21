package monad_core.engine.model

trait Locatable:
  def id: LocatableId

  def position: Vector2D

  def shape: Shape2D

  def rotation: Double

object Locatable:

  def circle[A](id: String, position: Vector2D, radius: Double, rotation: Double = 0)(
      build: (LocatableId, Vector2D, Shape2D, Double) => A
  ): Either[EngineError, A] =
    Shape2D.circle(radius).flatMap(circle => createGeneric(id, position, circle, rotation)(build))

  private def createGeneric[A](
      id: String,
      position: Vector2D,
      shape: Shape2D,
      rotation: Double
  )(build: (LocatableId, Vector2D, Shape2D, Double) => A): Either[EngineError, A] =
    for {
      locatableId <- LocatableId(id)
      _           <- validatePosition(position)
      _           <- validateRotation(rotation)
    } yield build(locatableId, position, shape, rotation)

  def validatePosition(position: Vector2D): Either[EngineError, Unit] =
    if position.x < 0 || position.y < 0 then Left(PositionIsValid(position))
    else Right(())

  def validateRotation(rotation: Double): Either[EngineError, Unit] =
    Either.cond(rotation >= 0 && rotation <= 360, (), RotationMustBeAValidDegreeValue(rotation))

  def rectangle[A](
      id: String,
      position: Vector2D,
      height: Double,
      length: Double,
      rotation: Double = 0
  )(build: (LocatableId, Vector2D, Shape2D, Double) => A): Either[EngineError, A] =
    Shape2D
      .rectangle(height, length)
      .flatMap(rectangle => createGeneric(id, position, rectangle, rotation)(build))
