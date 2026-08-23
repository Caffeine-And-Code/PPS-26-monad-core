package monad_core.engine.physics.utils

import monad_core.engine.geometry.Collision
import monad_core.engine.model.*
import monad_core.engine.physics.pathfinding.RectangleVertexes.vertexes

private[engine] case class BorderWallResult(
    wall: Entity,
    collision: Collision
)

private[engine] object BorderWall:

  private val LeftWallId   = "left-wall"
  private val RightWallId  = "right-wall"
  private val TopWallId    = "top-wall"
  private val BottomWallId = "bottom-wall"
  private val Epsilon      = 1e-9

  private val LeftCollisionVector   = Vector2D(1, 0)
  private val TopCollisionVector    = LeftCollisionVector.swap
  private val RightCollisionVector  = LeftCollisionVector.flip
  private val BottomCollisionVector = TopCollisionVector.flip

  private val WallStartPosition = Vector2D(0, 0)

  def apply(
      entity: Entity,
      horizontalHalfSize: Double,
      verticalHalfSize: Double,
      upperLeft: Vector2D,
      lowerRight: Vector2D,
      borderSide: BorderSide
  ): Either[EngineError, BorderWallResult] =

    val vertical   = verticalHalfSize
    val horizontal = horizontalHalfSize
    val position   = entity.position

    for wall <- wallSelector(position, upperLeft, lowerRight, horizontal, vertical, borderSide)
    yield
      val (normal, depth) = collisionVectorDepht(
        wall,
        upperLeft,
        lowerRight,
        borderSide
      )
      val point = collisionPoint(entity, upperLeft, lowerRight, borderSide)
      BorderWallResult(wall, Collision(normal, depth, point))

  private def collisionVectorDepht(
      wall: Entity,
      upperLeft: Vector2D,
      lowerRight: Vector2D,
      borderSide: BorderSide
  ): (Vector2D, Double) =
    borderSide match
      case BorderSide.Left =>
        (
          LeftCollisionVector,
          math.abs(wall.position.x - upperLeft.x)
        )
      case BorderSide.Right =>
        (
          RightCollisionVector,
          math.abs(wall.position.x - lowerRight.x)
        )
      case BorderSide.Top =>
        (
          TopCollisionVector,
          math.abs(wall.position.y - upperLeft.y)
        )
      case BorderSide.Bottom =>
        (
          BottomCollisionVector,
          math.abs(wall.position.y - lowerRight.y)
        )

  private def collisionPoint(
      entity: Entity,
      upperLeft: Vector2D,
      lowerRight: Vector2D,
      borderSide: BorderSide
  ): Vector2D =

    val supportCentre = entity.shape match
      case _: Shape2D.Circle =>
        entity.position
      case rectangle: Shape2D.Rectangle =>
        val supportDirection = borderSide match
          case BorderSide.Left   => RightCollisionVector
          case BorderSide.Right  => LeftCollisionVector
          case BorderSide.Top    => BottomCollisionVector
          case BorderSide.Bottom => TopCollisionVector

        val vertexes = rectangle.vertexes(entity.position, entity.rotation)

        val projections = vertexes.map(_ dot supportDirection)
        val maximum     = projections.max
        val supportVertices = vertexes
          .zip(projections)
          .collect { case (vertex, projection) if maximum - projection <= Epsilon => vertex }

        supportVertices.reduce(_ + _) * (1.0 / supportVertices.size)

    borderSide match
      case BorderSide.Left   => supportCentre.copy(x = upperLeft.x)
      case BorderSide.Right  => supportCentre.copy(x = lowerRight.x)
      case BorderSide.Top    => supportCentre.copy(y = upperLeft.y)
      case BorderSide.Bottom => supportCentre.copy(y = lowerRight.y)

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
      position = WallStartPosition,
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
      position = WallStartPosition,
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
      position = WallStartPosition,
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
      position = WallStartPosition,
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
