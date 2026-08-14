package monad_core.engine.physics.pathfinding

import monad_core.engine.model.*
import monad_core.engine.physics.core.{PhysicsError, RayIntersectedAMissingEntity, RayIntersectedNothing}

private[physics] object RayCast:

  private def WayPointDisplacement = 0.1

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

    val firstObject = RayIntersection(
      from.position,
      rayDirection,
      entitiesVertexesWithoutFrom
    )

    firstObject match
      case Some(first) =>
        if first == to.id then
          Right(Some(to.position))
        else
          findEncounteredEntityWayPoint(
            first,
            from,
            entities,
            entitiesVertexesWithoutFrom,
            upperLeftSceneCorner,
            lowerRightSceneCorner
          )
      case None => Left(RayIntersectedNothing(from.id.value, to.id.value))

  private def findEncounteredEntityWayPoint(
                            firstEncounteredEntity: LocatableId,
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
    val vertical = SizeHelper.verticalShapeSize(from) / 2 + WayPointDisplacement

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
    val vertical = SizeHelper.verticalShapeSize(from) / 2

    waypoint.x - horizontal >= upperLeftSceneCorner.x &&
      waypoint.y - vertical >= upperLeftSceneCorner.y &&
      waypoint.x + horizontal <= lowerRightSceneCorner.x &&
      waypoint.y + vertical <= lowerRightSceneCorner.y

  private def findBestWaypoint(
    to: Entity,
    from: Entity,
    waypoints: List[Vector2D],
    upperLeftSceneCorner: Vector2D,
    lowerRightSceneCorner: Vector2D
  ): Option[Vector2D] =

    val validWaypoints = waypoints
      .map(w => actualWaypoint(to, from, w))
      .filter(w => isValidWayPoint(
        to,
        from,
        w,
        upperLeftSceneCorner,
        lowerRightSceneCorner
      ))

    if validWaypoints.isEmpty then None
    else Some(validWaypoints.minBy(_.euclideanDistance(from.position)))