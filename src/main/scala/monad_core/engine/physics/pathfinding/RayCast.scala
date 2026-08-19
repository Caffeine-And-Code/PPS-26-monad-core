package monad_core.engine.physics.pathfinding

import monad_core.engine.model.*
import monad_core.engine.physics.core.{
  PhysicsError,
  RayIntersectedAMissingEntity,
  RayIntersectedNothing
}

private[physics] object RayCast:

  private val WayPointDisplacement = 5.0
  private val CornerDisplacement    = 1.0

  def apply(
      to: Entity,
      from: Entity,
      entities: List[Entity],
      entitiesVertexes: Map[LocatableId, List[Vector2D]],
      upperLeftSceneCorner: Vector2D,
      lowerRightSceneCorner: Vector2D
  ): Either[PhysicsError, Option[Vector2D]] =
    val entitiesVertexesWithoutFrom = entitiesVertexes.filterNot(_._1 == from.id)

    val rayDirection = (to.position - from.position).normalized
    val perpendicularDirection = Vector2D(-rayDirection.y, rayDirection.x)
    val hunterClearance = (math.max(
      SizeHelper.horizontalShapeSize(from),
      SizeHelper.verticalShapeSize(from)
    ) / 2) - CornerDisplacement

    val rayOrigins = List(
      from.position,
      from.position + perpendicularDirection * hunterClearance,
      from.position - perpendicularDirection * hunterClearance
    )

    val firstObjects = rayOrigins.flatMap { origin =>
      RayIntersection.withDistance(
        origin,
        rayDirection,
        entitiesVertexesWithoutFrom
      )
    }

    val firstObject = firstObjects
      .filter(_._1 != to.id)
      .minByOption(_._2)
      .orElse(firstObjects.find(_._1 == to.id))

    firstObject match
      case Some((first, _)) =>
        if first == to.id then Right(Some(to.position))
        else
          findEncounteredEntityWayPoint(
            first,
            to,
            from,
            entities,
            entitiesVertexesWithoutFrom,
            upperLeftSceneCorner,
            lowerRightSceneCorner
          )
      case None => Left(RayIntersectedNothing(from.id.value, to.id.value))

  private def findEncounteredEntityWayPoint(
      firstEncounteredEntity: LocatableId,
      originalTarget: Entity,
      from: Entity,
      entities: List[Entity],
      entitiesVertexes: Map[LocatableId, List[Vector2D]],
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

  private def actualWaypoint(
      to: Entity,
      from: Entity,
      waypoint: Vector2D
  ): Vector2D =

    val direction = (waypoint - to.position).normalized

    val module = (waypoint - to.position).magnitude

    val horizontal = SizeHelper.horizontalShapeSize(from) / 2 + WayPointDisplacement
    val vertical   = SizeHelper.verticalShapeSize(from) / 2 + WayPointDisplacement

    val entityDisplacement = Vector2D(
      if direction.x > 0 then horizontal else -horizontal,
      if direction.y > 0 then vertical else -vertical
    )

    val totalDisplacement = direction * entityDisplacement.magnitude

    waypoint + totalDisplacement

  private def isValidWayPoint(
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
