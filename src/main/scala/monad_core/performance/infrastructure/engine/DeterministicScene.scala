package monad_core.performance.infrastructure.engine

import monad_core.engine.model.{Entity, Scene, Vector2D, WorldBounds}
import monad_core.performance.domain.{EnginePerformanceError, EntityCount, PerformanceError}

/**
 * Builds reproducible engine scenes for performance measurements.
 *
 * Entities are equal circles arranged row by row on a square-like grid, assigned stable identifiers,
 * and given the same horizontal speed. World dimensions are derived from the grid so every entity
 * starts inside the bounds. This removes random scene variation between experiment runs.
 */
object DeterministicScene:

  /** Radius assigned to every generated circle. */
  private val EntityRadius = 1.0

  /** Distance between the origins of adjacent grid cells. */
  private val EntitySpacing = EntityRadius * 4.0

  /** Padding between the world origin and the first entity center. */
  private val OriginOffset = EntitySpacing

  /** Horizontal speed assigned to every generated entity. */
  private val HorizontalSpeed = 1.0

  /** Stable prefix used to derive unique entity identifiers. */
  private val EntityIdPrefix = "performance-entity"

  /**
   * Creates a deterministic scene containing exactly the requested number of entities.
   *
   * @param entityCount
   *   validated number of entities to create
   * @return
   *   the populated scene, or an
   *   [[monad_core.performance.domain.EnginePerformanceError]] if bounds or an entity cannot be
   *   built or inserted
   */
  def apply(entityCount: EntityCount): Either[PerformanceError, Scene] =
    for
      bounds <- boundsFor(entityCount)
      scene  <- addEntities(Scene(bounds = bounds), entityCount)
    yield scene

  /**
   * Calculates world bounds large enough for the generated entity grid.
   *
   * @param entityCount
   *   number of grid cells that must fit in the world
   * @return
   *   valid world bounds or a translated engine validation error
   */
  private def boundsFor(entityCount: EntityCount): Either[PerformanceError, WorldBounds] =
    val columns = columnsFor(entityCount)
    val rows    = math.ceil(entityCount.value.toDouble / columns).toInt
    val width   = dimensionFor(columns)
    val height  = dimensionFor(rows)
    WorldBounds(width, height).left.map(EnginePerformanceError.apply)

  /**
   * Adds every deterministic entity to an initial scene in index order.
   *
   * The fold is fail-fast: once entity creation or insertion fails, the error is propagated and no
   * later entity is attempted.
   *
   * @param initialScene
   *   empty or partially initialized scene receiving generated entities
   * @param entityCount
   *   number of entities to generate
   * @return
   *   the fully populated immutable scene or the first translated engine error
   */
  private def addEntities(
      initialScene: Scene,
      entityCount: EntityCount
  ): Either[PerformanceError, Scene] =
    (0 until entityCount.value).foldLeft(
      Right(initialScene): Either[PerformanceError, Scene]
    ) { (sceneResult, index) =>
      for
        scene  <- sceneResult
        entity <- entityAt(index, columnsFor(entityCount))
        updatedScene <- scene
          .addEntity(entity)
          .left
          .map(EnginePerformanceError.apply)
      yield updatedScene
    }

  /**
   * Creates the entity assigned to one row-major grid index.
   *
   * @param index
   *   zero-based entity and grid-cell index
   * @param columns
   *   positive number of columns in the grid
   * @return
   *   a moving circle with deterministic id and position, or a translated engine validation error
   */
  private def entityAt(index: Int, columns: Int): Either[PerformanceError, Entity] =
    val column   = index % columns
    val row      = index / columns
    val position = Vector2D(coordinateFor(column), coordinateFor(row))

    Entity
      .circle(
        id = s"$EntityIdPrefix-$index",
        position = position,
        radius = EntityRadius
      )
      .map(_.withSpeed(Vector2D(HorizontalSpeed, 0.0)))
      .left
      .map(EnginePerformanceError.apply)

  /**
   * Selects the column count for a grid as close to square as possible.
   *
   * @param entityCount
   *   number of cells to arrange
   * @return
   *   ceiling of the square root of the entity count
   */
  private def columnsFor(entityCount: EntityCount): Int =
    math.ceil(math.sqrt(entityCount.value.toDouble)).toInt

  /**
   * Converts a zero-based grid coordinate into a world-space center coordinate.
   *
   * @param index
   *   row or column index
   * @return
   *   coordinate offset from the origin by the configured padding and spacing
   */
  private def coordinateFor(index: Int): Double =
    OriginOffset + index * EntitySpacing

  /**
   * Calculates a world dimension for one grid axis.
   *
   * @param cellCount
   *   number of cells along the axis
   * @return
   *   dimension including entity spacing and padding on both sides
   */
  private def dimensionFor(cellCount: Int): Double =
    OriginOffset * 2.0 + cellCount * EntitySpacing
