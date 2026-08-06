package helpers.arrangers

import monad_core.simulator.domain.engine.MonadCoreShape.{SimulationCircle, SimulationRectangle}
import monad_core.simulator.domain.engine.{MonadCoreShape, MonadCoreSurface}

object MonadCoreSurfaceArranger:
  val RedSurfaceId: String = "RedSurface"
  val BlueSurfaceId: String = "BlueSurface"

  val DefaultPosition: (Double, Double) = (0.0, 0.0)
  val DefaultCircleRadius: Double = 2.0
  val DefaultRectangleWidth: Double = 7.0
  val DefaultRectangleHeight: Double = 3.0
  val DefaultFrictionIndex: Double = 0.3
  val DefaultAppliedForce: (Double, Double) = (2.0, -1.0)

  def arrangeSurfaces: Seq[MonadCoreSurface] =
    Seq(
      arrangeRedSurface(),
      arrangeBlueSurface()
    )

  def shapeFor(shapeKind: ShapeKind): MonadCoreShape =
    shapeKind match
      case ShapeKind.Circle => SimulationCircle(DefaultCircleRadius)
      case ShapeKind.Rectangle => SimulationRectangle(width = DefaultRectangleWidth, height = DefaultRectangleHeight)

  private def arrangeSurfaceWithoutOptionals(id: String, shapeKind: ShapeKind): MonadCoreSurface =
    MonadCoreSurface(id = id, position = DefaultPosition, shape = shapeFor(shapeKind))

  private def arrangeSurfaceWithOptionals(id: String, shapeKind: ShapeKind): MonadCoreSurface =
    MonadCoreSurface(
      id = id,
      position = DefaultPosition,
      shape = shapeFor(shapeKind),
      frictionIndex = Some(DefaultFrictionIndex),
      appliedForce = Some(DefaultAppliedForce)
    )

  def arrangeRedSurface(shapeKind: ShapeKind = ShapeKind.Circle, withOptionals: Boolean = false): MonadCoreSurface =
    if withOptionals
    then
      arrangeSurfaceWithOptionals(RedSurfaceId, shapeKind)
    else
      arrangeSurfaceWithoutOptionals(RedSurfaceId, shapeKind)

  def arrangeBlueSurface(shapeKind: ShapeKind = ShapeKind.Circle, withOptionals: Boolean = true): MonadCoreSurface =
    if withOptionals
    then
      arrangeSurfaceWithOptionals(BlueSurfaceId, shapeKind)
    else
      arrangeSurfaceWithoutOptionals(BlueSurfaceId, shapeKind)