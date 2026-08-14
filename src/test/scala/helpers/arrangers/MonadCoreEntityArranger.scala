package helpers.arrangers

import monad_core.simulator.domain.engine.MonadCoreShape.{SimulationCircle, SimulationRectangle}
import monad_core.simulator.domain.engine.{MonadCoreEntity, MonadCoreShape}

object MonadCoreEntityArranger:
  val RedEntityId: String  = "RedEntity"
  val BlueEntityId: String = "BlueEntity"

  val DefaultPosition: (Double, Double) = (0.0, 0.0)
  val DefaultCircleRadius: Double       = 2.0
  val DefaultRectangleWidth: Double     = 7.0
  val DefaultRectangleHeight: Double    = 3.0
  val DefaultSpeed: (Double, Double)    = (1.0, 1.0)
  val DefaultWeight: Int                = 10
  val DefaultHealth: Int                = 100
  val DefaultTeamId: String             = MonadCoreTeamArranger.RedTeamId

  def arrangeEntities: Seq[MonadCoreEntity] =
    Seq(
      arrangeRedEntity(),
      arrangeBlueEntity()
    )

  def shapeFor(shapeKind: ShapeKind): MonadCoreShape =
    shapeKind match
      case ShapeKind.Circle => SimulationCircle(DefaultCircleRadius)
      case ShapeKind.Rectangle =>
        SimulationRectangle(width = DefaultRectangleWidth, height = DefaultRectangleHeight)

  private def arrangeEntityWithoutOptionals(id: String, shapeKind: ShapeKind): MonadCoreEntity =
    MonadCoreEntity(id = id, position = DefaultPosition, shape = shapeFor(shapeKind))

  private def arrangeEntityWithOptionals(id: String, shapeKind: ShapeKind): MonadCoreEntity =
    MonadCoreEntity(
      id = id,
      position = DefaultPosition,
      shape = shapeFor(shapeKind),
      speed = Some(DefaultSpeed),
      weight = Some(DefaultWeight),
      health = Some(DefaultHealth),
      teamId = Some(DefaultTeamId)
    )

  def arrangeRedEntity(
      shapeKind: ShapeKind = ShapeKind.Circle,
      withOptionals: Boolean = false
  ): MonadCoreEntity =
    if withOptionals
    then arrangeEntityWithOptionals(RedEntityId, shapeKind)
    else arrangeEntityWithoutOptionals(RedEntityId, shapeKind)

  def arrangeBlueEntity(
      shapeKind: ShapeKind = ShapeKind.Circle,
      withOptionals: Boolean = true
  ): MonadCoreEntity =
    if withOptionals
    then arrangeEntityWithOptionals(BlueEntityId, shapeKind)
    else arrangeEntityWithoutOptionals(BlueEntityId, shapeKind)
