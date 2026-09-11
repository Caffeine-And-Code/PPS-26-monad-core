package monad_core.simulator.presentation.components.ai

import scalafx.scene.Node

/**
 * A ScalaFX node paired with a callback for updating the state of the component.
 *
 * @tparam Model component state type
 * @tparam View ScalaFX node type
 * @param view node owned by the component
 * @param render callback for updating the model state
 */
final case class Component[-Model, +View <: Node](
    view: View,
    render: Model => Unit
)
