package monad_core.engine.physics.utils

import monad_core.engine.geometry.Collision
import monad_core.engine.model.*
import monad_core.engine.physics.pathfinding.RectangleVertexes.vertexes

/**
 * Synthetic wall and collision generated for one crossed world border.
 *
 * @param wall
 *   fixed entity representing the crossed border
 * @param collision
 *   contact information between the entity and the border
 */
private[engine] case class BorderWallResult(
    wall: Entity,
    collision: Collision
)

/** Builds synthetic wall contacts for entities crossing the world bounds. */
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

  /**
   * Creates the wall and collision associated with one crossed border.
   *
   * @param entity
   *   entity crossing the world boundary
   * @param horizontalHalfSize
   *   half of the entity's horizontal extent
   * @param verticalHalfSize
   *   half of the entity's vertical extent
   * @param upperLeft
   *   upper-left world boundary
   * @param lowerRight
   *   lower-right world boundary
   * @param borderSide
   *   crossed border
   * @return
   *   generated contact, or the error produced while building its wall entity
   */
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

  /**
   * Calculates the inward collision normal and penetration depth for a border.
   *
   * @param wall
   *   synthetic wall crossing the selected border
   * @param upperLeft
   *   upper-left world boundary
   * @param lowerRight
   *   lower-right world boundary
   * @param borderSide
   *   crossed border
   * @return
   *   inward unit normal and positive penetration depth
   */
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

  /**
   * Calculates the world-space contact point on the selected border.
   * For rectangles, equally extreme support vertices are averaged before projection.
   *
   * @param entity
   *   entity crossing the border
   * @param upperLeft
   *   upper-left world boundary
   * @param lowerRight
   *   lower-right world boundary
   * @param borderSide
   *   crossed border
   * @return
   *   contact point projected onto the border
   */
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

  /**
   * Moves a successfully constructed wall while preserving construction errors.
   *
   * @param wall
   *   wall construction result
   * @param position
   *   desired wall center
   * @return
   *   moved wall, or the original construction error
   */
  private def moveWall(
      wall: Either[EngineError, Entity],
      position: Vector2D
  ): Either[EngineError, Entity] =
    wall match
      case Right(w)  => Right(w.moveTo(position))
      case Left(err) => Left(err)

  /**
   * Builds the synthetic wall extending beyond the left border.
   *
   * @param position
   *   crossing entity center
   * @param upperLeft
   *   upper-left world boundary
   * @param horizontal
   *   entity horizontal half-size
   * @param vertical
   *   entity vertical half-size
   * @return
   *   synthetic wall, or an [[EngineError]]
   */
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

  /**
   * Builds the synthetic wall extending beyond the right border.
   *
   * @param position
   *   crossing entity center
   * @param lowerRight
   *   lower-right world boundary
   * @param horizontal
   *   entity horizontal half-size
   * @param vertical
   *   entity vertical half-size
   * @return
   *   synthetic wall, or an [[EngineError]]
   */
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

  /**
   * Builds the synthetic wall extending beyond the top border.
   *
   * @param position
   *   crossing entity center
   * @param upperLeft
   *   upper-left world boundary
   * @param horizontal
   *   entity horizontal half-size
   * @param vertical
   *   entity vertical half-size
   * @return
   *   synthetic wall, or an [[EngineError]]
   */
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

  /**
   * Builds the synthetic wall extending beyond the bottom border.
   *
   * @param position
   *   crossing entity center
   * @param lowerRight
   *   lower-right world boundary
   * @param horizontal
   *   entity horizontal half-size
   * @param vertical
   *   entity vertical half-size
   * @return
   *   synthetic wall, or an [[EngineError]]
   */
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

  /**
   * Selects the synthetic wall constructor associated with a border side.
   *
   * @param position
   *   crossing entity center
   * @param upperLeft
   *   upper-left world boundary
   * @param lowerRight
   *   lower-right world boundary
   * @param horizontal
   *   entity horizontal half-size
   * @param vertical
   *   entity vertical half-size
   * @param borderSide
   *   crossed border
   * @return
   *   selected synthetic wall, or an [[EngineError]]
   */
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
