package engine.model

private [model] trait Locatable:
  def id: LocatableId
  def position: Vector2D
  def shape: Shape2D


object Locatable:
  def validate(position: Vector2D): Either[String, Unit] =
    if position.x < 0 || position.y < 0 then
      Left("Position is invalid, x and y should be greater then 0")
    else
      Right(())

  private def createGeneric[A](
                                id: String,
                                position: Vector2D,
                                shape: Shape2D
                              )(build: (LocatableId, Vector2D, Shape2D) => A): Either[String, A] =
    LocatableId(id).flatMap(lId => validate(position).map(_ => build(lId, position, shape)))

  def circle[A](id: String, position: Vector2D, radius: Double)(build: (LocatableId, Vector2D, Shape2D) => A): Either[String, A] =
    createGeneric(id, position, Shape2D.Circle(radius))(build)

  def rectangle[A](id: String, position: Vector2D, height: Double, length: Double)(build: (LocatableId, Vector2D, Shape2D) => A): Either[String, A] =
    createGeneric(id, position, Shape2D.Rectangle(height, length))(build)
