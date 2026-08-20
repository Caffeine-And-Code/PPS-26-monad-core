package monad_core.engine.physics.pathfinding

import monad_core.engine.model.*
import monad_core.engine.physics.core.{
  PhysicsError,
  RayIntersectedAMissingEntity,
  RayIntersectedNothing
}

private[physics] object RayCast:

  private val WayPointDisplacement = 5.0
  private val HunterMargin         = 1.0

  def apply(
      to: Entity,
      from: Entity,
      entities: List[Entity],
      entitiesVertexes: Map[LocatableId, List[Vector2D]],
      upperLeftSceneCorner: Vector2D,
      lowerRightSceneCorner: Vector2D
  ): Either[PhysicsError, Option[Vector2D]] =
    val entitiesVertexesWithoutFrom = entitiesVertexes.filterNot(_._1 == from.id)
    val inflatedVertexes = inflateVertexes(
      entitiesVertexesWithoutFrom,
      entities,
      hunterRadius(from) + HunterMargin
    )

    val rayDirection = (to.position - from.position).normalized
    val firstObject = RayIntersection.withDistance(
      from.position,
      rayDirection,
      inflatedVertexes
    )

    firstObject match
      case Some((first, _)) =>
        if first == to.id then Right(Some(to.position))
        else
          findEncounteredEntityWayPoint(
            first,
            to,
            from,
            entities,
            upperLeftSceneCorner,
            lowerRightSceneCorner
          )
      case None => Left(RayIntersectedNothing(from.id.value, to.id.value))

  private def findEncounteredEntityWayPoint(
      firstEncounteredEntity: LocatableId,
      originalTarget: Entity,
      from: Entity,
      entities: List[Entity],
      upperLeftSceneCorner: Vector2D,
      lowerRightSceneCorner: Vector2D
  ): Either[PhysicsError, Option[Vector2D]] =

    val targetEntity = entities.find(_.id == firstEncounteredEntity)

    targetEntity match
      case Some(target) =>
        val waypoints = WaypointFinder(from, target)

        val bestWaypoint = findBestWaypoint(
          target,
          originalTarget,
          from,
          waypoints,
          upperLeftSceneCorner,
          lowerRightSceneCorner
        )

        Right(bestWaypoint)
      case None => Left(RayIntersectedAMissingEntity(firstEncounteredEntity.value))

  private[pathfinding] def hunterRadius(hunter: Entity): Double =
    hunter.shape match
      case circle: Shape2D.Circle => circle.radius
      case _: Shape2D.Rectangle =>
        val halfWidth  = SizeHelper.horizontalShapeSize(hunter) / 2
        val halfHeight = SizeHelper.verticalShapeSize(hunter) / 2
        Vector2D(halfWidth, halfHeight).magnitude

  private[pathfinding] def inflateVertexes(
      vertexes: Map[LocatableId, List[Vector2D]],
      entities: List[Entity],
      inflation: Double
  ): Map[LocatableId, List[Vector2D]] =
    val entitiesById = entities.map(entity => entity.id -> entity).toMap

    vertexes.map { case (id, originalVertexes) =>
      val inflated = entitiesById.get(id).fold(originalVertexes) { entity =>
        entity.shape match
          case _: Shape2D.Circle =>
            originalVertexes.map { vertex =>
              vertex + (vertex - entity.position).normalized * inflation
            }
          case _: Shape2D.Rectangle =>
            originalVertexes.map { vertex =>
              val direction = vertex - entity.position
              Vector2D(
                vertex.x + math.signum(direction.x) * inflation,
                vertex.y + math.signum(direction.y) * inflation
              )
            }
      }

      id -> inflated
    }

  private[pathfinding] def actualWaypoint(
      to: Entity,
      from: Entity,
      waypoint: Vector2D
  ): Vector2D =

    val displace: Double => Double = _ / 2.0 + WayPointDisplacement
    val horizontal = displace(SizeHelper.horizontalShapeSize(from))
    val vertical   = displace(SizeHelper.verticalShapeSize(from))

    to.shape match
      case _: Shape2D.Rectangle =>
        val cornerDirection = waypoint - to.position
        waypoint + Vector2D(
          math.signum(cornerDirection.x) * horizontal,
          math.signum(cornerDirection.y) * vertical
        )
      case _: Shape2D.Circle =>
        val direction             = (waypoint - to.position).normalized
        val displacementMagnitude = Vector2D(horizontal, vertical).magnitude
        waypoint + direction * displacementMagnitude

  private[pathfinding] def isValidWayPoint(
      to: Entity,
      from: Entity,
      waypoint: Vector2D,
      upperLeftSceneCorner: Vector2D,
      lowerRightSceneCorner: Vector2D
  ): Boolean =

    val horizontal = SizeHelper.horizontalShapeSize(from) / 2
    val vertical   = SizeHelper.verticalShapeSize(from) / 2

    waypoint.x - horizontal >= upperLeftSceneCorner.x &&
    waypoint.y - vertical >= upperLeftSceneCorner.y &&
    waypoint.x + horizontal <= lowerRightSceneCorner.x &&
    waypoint.y + vertical <= lowerRightSceneCorner.y

  private def findBestWaypoint(
      to: Entity,
      originalTarget: Entity,
      from: Entity,
      waypoints: List[Vector2D],
      upperLeftSceneCorner: Vector2D,
      lowerRightSceneCorner: Vector2D
  ): Option[Vector2D] =

    val validWaypoints = waypoints
      .map(w => actualWaypoint(to, from, w))
      .filter(w =>
        isValidWayPoint(
          to,
          from,
          w,
          upperLeftSceneCorner,
          lowerRightSceneCorner
        )
      )

    if validWaypoints.isEmpty then None
    else Some(validWaypoints.minBy(_.euclideanDistance(originalTarget.position)))
