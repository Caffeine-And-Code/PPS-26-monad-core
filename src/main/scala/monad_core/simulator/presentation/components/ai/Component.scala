package monad_core.simulator.presentation.components.ai

import scalafx.scene.Node

/** A ScalaFX node paired with the effect that renders an immutable model. */
final case class Component[-Model, +View <: Node](
    view: View,
    render: Model => Unit
)
