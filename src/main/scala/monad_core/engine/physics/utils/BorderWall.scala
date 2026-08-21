package monad_core.engine.physics.utils

import monad_core.engine.geometry.Collision
import monad_core.engine.model.{BorderSide, EngineError, Entity, Vector2D}

object BorderWall:

  private val LeftWallId   = "left-wall"
  private val RightWallId  = "right-wall"
  private val TopWallId    = "top-wall"
  private val BottomWallId = "bottom-wall"

  def apply(
      entity: Entity,
      horizontalHalfSize: Double,
      verticalHalfSize: Double,
      upperLeft: Vector2D,
      lowerRight: Vector2D,
      borderSide: BorderSide
  ): Either[EngineError, (Entity, Collision)] =

    val vertical   = verticalHalfSize
    val horizontal = horizontalHalfSize
    val position   = entity.position

    for
      wall <- wallSelector(position, upperLeft, lowerRight, horizontal, vertical, borderSide)

      normal = borderSide match
        case BorderSide.Left   => Vector2D(1, 0)
        case BorderSide.Right  => Vector2D(-1, 0)
        case BorderSide.Top    => Vector2D(0, 1)
        case BorderSide.Bottom => Vector2D(0, -1)

      depth = borderSide match
        case BorderSide.Left   => math.abs(wall.position.x - upperLeft.x)
        case BorderSide.Right  => math.abs(wall.position.x - lowerRight.x)
        case BorderSide.Top    => math.abs(wall.position.y - upperLeft.y)
        case BorderSide.Bottom => math.abs(wall.position.y - lowerRight.y)
    yield (wall, Collision(normal, depth))

  private def moveWall(
      wall: Either[EngineError, Entity],
      position: Vector2D
  ): Either[EngineError, Entity] =
    wall match
      case Right(w)  => Right(w.moveTo(position))
      case Left(err) => Left(err)

  private def leftWall(
      position: Vector2D,
      upperLeft: Vector2D,
      horizontal: Double,
      vertical: Double
  ): Either[EngineError, Entity] =

    val muchExternalPoint = position.x - horizontal

    val wall = Entity.rectangle(
      id = LeftWallId,
      position = Vector2D(0, 0),
      length = math.abs(muchExternalPoint - upperLeft.x) * 2,
      height = vertical * 2
    )

    moveWall(wall, Vector2D(muchExternalPoint, position.y))

  private def rightWall(
      position: Vector2D,
      lowerRight: Vector2D,
      horizontal: Double,
      vertical: Double
  ): Either[EngineError, Entity] =

    val muchExternalPoint = position.x + horizontal

    val wall = Entity.rectangle(
      id = RightWallId,
      position = Vector2D(0.0, 0.0),
      length = math.abs(muchExternalPoint - lowerRight.x) * 2,
      height = vertical * 2
    )

    moveWall(wall, Vector2D(muchExternalPoint, position.y))

  private def topWall(
      position: Vector2D,
      upperLeft: Vector2D,
      horizontal: Double,
      vertical: Double
  ): Either[EngineError, Entity] =
    val muchExternalPoint = position.y - vertical

    val wall = Entity.rectangle(
      id = TopWallId,
      position = Vector2D(0.0, 0.0),
      length = horizontal * 2,
      height = math.abs(muchExternalPoint - upperLeft.y) * 2
    )

    moveWall(wall, Vector2D(position.x, muchExternalPoint))

  private def bottomWall(
      position: Vector2D,
      lowerRight: Vector2D,
      horizontal: Double,
      vertical: Double
  ): Either[EngineError, Entity] =
    val muchExternalPoint = position.y + vertical

    val wall = Entity.rectangle(
      id = BottomWallId,
      position = Vector2D(0.0, 0.0),
      length = horizontal * 2,
      height = math.abs(muchExternalPoint - lowerRight.y) * 2
    )

    moveWall(wall, Vector2D(position.x, muchExternalPoint))

  private def wallSelector(
      position: Vector2D,
      upperLeft: Vector2D,
      lowerRight: Vector2D,
      horizontal: Double,
      vertical: Double,
      borderSide: BorderSide
  ): Either[EngineError, Entity] =
    borderSide match
      case BorderSide.Left =>
        leftWall(
          position = position,
          upperLeft = upperLeft,
          horizontal = horizontal,
          vertical = vertical
        )
      case BorderSide.Right =>
        rightWall(
          position = position,
          lowerRight = lowerRight,
          horizontal = horizontal,
          vertical = vertical
        )
      case BorderSide.Top =>
        topWall(
          position = position,
          upperLeft = upperLeft,
          horizontal = horizontal,
          vertical = vertical
        )
      case BorderSide.Bottom =>
        bottomWall(
          position = position,
          lowerRight = lowerRight,
          horizontal = horizontal,
          vertical = vertical
        )
