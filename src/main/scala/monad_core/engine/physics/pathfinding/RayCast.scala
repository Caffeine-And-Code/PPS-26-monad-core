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
      case rectangle: Shape2D.Rectangle =>
        Vector2D(rectangle.halfLength, rectangle.halfHeight).magnitude

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
              val localDirection = (vertex - entity.position).rotated(-entity.rotation)
              val localInflation = Vector2D(
                math.signum(localDirection.x) * inflation,
                math.signum(localDirection.y) * inflation
              )
              vertex + localInflation.rotated(entity.rotation)
            }
      }

      id -> inflated
    }

  private[pathfinding] def actualWaypoint(
      to: Entity,
      from: Entity,
      waypoint: Vector2D
  ): Vector2D =

    val clearance = hunterRadius(from) + WayPointDisplacement

    to.shape match
      case _: Shape2D.Rectangle =>
        val localDirection = (waypoint - to.position).rotated(-to.rotation)
        val localDisplacement = Vector2D(
          math.signum(localDirection.x) * clearance,
          math.signum(localDirection.y) * clearance
        )
        waypoint + localDisplacement.rotated(to.rotation)
      case _: Shape2D.Circle =>
        val direction = (waypoint - to.position).normalized
        waypoint + direction * clearance

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
