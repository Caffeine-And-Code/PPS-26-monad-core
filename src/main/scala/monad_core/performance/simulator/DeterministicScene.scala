package monad_core.performance.simulator

import monad_core.engine.model.*
import monad_core.performance.model.{EnginePerformanceError, EntityCount, PerformanceError}

/** Reproducible full-physics scene used by engine performance measurements. */
object DeterministicScene:
  private val EntityRadius               = 1.0
  private val EntitySide                 = EntityRadius * 2.0
  private val EntitySpacing              = EntitySide * 0.75
  private val BorderOffset               = EntityRadius / 2.0
  private val EntityWeight               = 2
  private val EntityHealth               = 100
  private val EntityDamage               = 1
  private val LinearSpeed                = 1.0
  private val VerticalDirectionBlockSize = 2
  private val AngularSpeed               = 30.0
  private val RotationStep               = 15.0
  private val RotationStepsPerTurn       = 24
  private val EntityIdPrefix             = "performance-entity"
  private val FirstTeamId                = "performance-team-a"
  private val SecondTeamId               = "performance-team-b"
  private val SurfaceId                  = "performance-surface"
  private val SurfaceFriction            = 0.1
  private val SurfaceForce               = Vector2D(0.25, 0.5)
  private val SurfaceDamage              = 1

  def apply(entityCount: EntityCount): Either[PerformanceError, Scene] =
    for
      bounds           <- boundsFor(entityCount)
      sceneWithTeams   <- addTeams(Scene(bounds = bounds))
      surface          <- surfaceFor(bounds)
      sceneWithSurface <- fromEngine(sceneWithTeams.addSurface(surface))
      populatedScene   <- addEntities(sceneWithSurface, entityCount)
    yield populatedScene

  private def fromEngine[A](result: Either[EngineError, A]): Either[PerformanceError, A] =
    result.left.map(EnginePerformanceError.apply)

  private def boundsFor(entityCount: EntityCount): Either[PerformanceError, WorldBounds] =
    val columns = columnsFor(entityCount)
    val rows    = math.ceil(entityCount.value.toDouble / columns).toInt
    fromEngine(WorldBounds(dimensionFor(columns), dimensionFor(rows)))

  private def addTeams(initialScene: Scene): Either[PerformanceError, Scene] =
    for
      firstTeam  <- fromEngine(Team.create(FirstTeamId, Set(SecondTeamId)))
      secondTeam <- fromEngine(Team.create(SecondTeamId, Set(FirstTeamId)))
      withFirst  <- fromEngine(initialScene.addTeam(firstTeam))
      withBoth   <- fromEngine(withFirst.addTeam(secondTeam))
    yield withBoth

  private def surfaceFor(bounds: WorldBounds): Either[PerformanceError, Surface] =
    val width  = bounds.lowerRight.x - bounds.upperLeft.x
    val height = bounds.lowerRight.y - bounds.upperLeft.y
    val center = Vector2D(
      bounds.upperLeft.x + width / 2.0,
      bounds.upperLeft.y + height / 2.0
    )

    fromEngine(
      for
        surface      <- Surface.rectangle(SurfaceId, center, height, width)
        withFriction <- surface.withFrictionIndex(SurfaceFriction)
        withForce    <- withFriction.withAppliedForce(SurfaceForce)
        withDamage   <- withForce.withDamageOverTime(SurfaceDamage)
      yield withDamage
    )

  private def addEntities(
      initialScene: Scene,
      entityCount: EntityCount
  ): Either[PerformanceError, Scene] =
    val columns = columnsFor(entityCount)

    (0 until entityCount.value).foldLeft(
      Right(initialScene): Either[PerformanceError, Scene]
    ) { (sceneResult, index) =>
      for
        scene        <- sceneResult
        entity       <- entityAt(index, columns)
        updatedScene <- fromEngine(scene.addEntity(entity))
      yield updatedScene
    }

  private def entityAt(index: Int, columns: Int): Either[PerformanceError, Entity] =
    val position = Vector2D(
      coordinateFor(index % columns),
      coordinateFor(index / columns)
    )
    val rotation = (index % RotationStepsPerTurn) * RotationStep

    for
      entity       <- fromEngine(entityShapeAt(index, position, rotation))
      weighted     <- fromEngine(entity.withWeight(EntityWeight))
      healthy      <- fromEngine(weighted.withHealth(EntityHealth))
      damaging     <- fromEngine(healthy.withDamage(EntityDamage))
      assignedTeam <- fromEngine(damaging.withTeamId(teamIdFor(index)))
    yield assignedTeam
      .withSpeed(speedFor(index))
      .withAngularSpeed(angularSpeedFor(index))

  private def entityShapeAt(
      index: Int,
      position: Vector2D,
      rotation: Double
  ): Either[EngineError, Entity] =
    if isEven(index) then Entity.circle(entityIdFor(index), position, EntityRadius, rotation)
    else Entity.rectangle(entityIdFor(index), position, EntitySide, EntitySide, rotation)

  private def speedFor(index: Int): Vector2D =
    Vector2D(
      directionFor(index) * LinearSpeed,
      directionFor(index / VerticalDirectionBlockSize) * LinearSpeed
    )

  private def angularSpeedFor(index: Int): Double =
    directionFor(index) * AngularSpeed

  private def teamIdFor(index: Int): String =
    if isEven(index) then FirstTeamId else SecondTeamId

  private def entityIdFor(index: Int): String =
    s"$EntityIdPrefix-$index"

  private def directionFor(block: Int): Double =
    if isEven(block) then 1.0 else -1.0

  private def isEven(value: Int): Boolean =
    value % 2 == 0

  private def columnsFor(entityCount: EntityCount): Int =
    math.ceil(math.sqrt(entityCount.value.toDouble)).toInt

  private def coordinateFor(index: Int): Double =
    BorderOffset + index * EntitySpacing

  private def dimensionFor(cellCount: Int): Double =
    BorderOffset * 2.0 + (cellCount - 1) * EntitySpacing
