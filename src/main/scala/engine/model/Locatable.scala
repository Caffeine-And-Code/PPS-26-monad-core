package engine.model

private [model] trait Locatable:
  def id: String
  def position: Vector2D
  def shape: Shape2D


object Locatable:
  def validate(id: String, position: Vector2D): Either[String, Unit] =
    if id.trim.isEmpty then
      Left("ID cannot be empty")
    else if position.x <= 0 || position.y <= 0 then
      Left("Position cannot be empty")
    else
      Right(())

  private def createGeneric[A](
                                id: String,
                                position: Vector2D,
                                shape: Shape2D
                              )(build: (String, Vector2D, Shape2D) => A): Either[String, A] =
    validate(id, position).map(_ => build(id, position, shape))

  def circle[A](id: String, position: Vector2D, radius: Double)(build: (String, Vector2D, Shape2D) => A): Either[String, A] =
    createGeneric(id, position, Shape2D.Circle(radius))(build)

  def rectangle[A](id: String, position: Vector2D, height: Double, length: Double)(build: (String, Vector2D, Shape2D) => A): Either[String, A] =
    createGeneric(id, position, Shape2D.Rectangle(height, length))(build)

