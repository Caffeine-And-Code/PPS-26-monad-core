package engine.model

import engine.errors.EngineError

trait Locatable:
  def id: LocatableId

  def position: Vector2D

  def shape: Shape2D

object Locatable:
  def circle[A](id: String, position: Vector2D, radius: Double)(build: (LocatableId, Vector2D, Shape2D) => A): Either[EngineError, A] =
    Shape2D.circle(radius).flatMap(circle => createGeneric(id, position, circle)(build))

  def rectangle[A](id: String, position: Vector2D, height: Double, length: Double)(build: (LocatableId, Vector2D, Shape2D) => A): Either[EngineError, A] =
    Shape2D.rectangle(height, length).flatMap(rectangle => createGeneric(id, position, rectangle)(build))

  private def createGeneric[A](
                                id: String,
                                position: Vector2D,
                                shape: Shape2D
                              )(build: (LocatableId, Vector2D, Shape2D) => A): Either[EngineError, A] =
    LocatableId(id).flatMap(lId => validate(position).map(_ => build(lId, position, shape)))

  def validate(position: Vector2D): Either[EngineError, Unit] =
    if position.x < 0 || position.y < 0 then
      Left(PositionIsValid(position))
    else
      Right(())
