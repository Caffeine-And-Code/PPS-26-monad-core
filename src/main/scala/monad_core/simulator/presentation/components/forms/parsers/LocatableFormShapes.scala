package monad_core.simulator.presentation.components.forms.parsers

import monad_core.simulator.domain.engine.MonadCoreShape
import monad_core.simulator.domain.engine.MonadCoreShape.{SimulationCircle, SimulationRectangle}

private[forms] enum LocatableFormShapes:
  private[forms] case Circle
  private[forms] case Rectangle

private[forms] object LocatableFormShapes:
  val CircleLabel = "Circle"
  val RectangleLabel = "Rectangle"

  extension (shape: MonadCoreShape)
    def getEnumValue: LocatableFormShapes =
      shape match
        case SimulationCircle(_) => LocatableFormShapes.Circle
        case SimulationRectangle(_, _) => LocatableFormShapes.Rectangle

    def getDefaultValuesByShape: (Option[String], Option[String], Option[String]) =
      shape match
        case SimulationCircle(r) => (Some(r.toString), None, None)
        case SimulationRectangle(w, h) => (None, Some(w.toString), Some(h.toString))

  extension (value: LocatableFormShapes)
    def getStringValue: String =
      value match
        case Circle => CircleLabel
        case Rectangle => RectangleLabel