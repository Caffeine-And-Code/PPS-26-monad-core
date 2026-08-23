package monad_core.simulator.presentation.components.forms.base

import scalafx.scene.Node

final private[forms] case class FormFieldsState(
    activeInputs: Map[String, () => String] = Map.empty,
    dynamicNodes: Seq[Node] = Seq.empty
)

private[forms] object FormFieldsState:

  extension (state: FormFieldsState)

    def withField(
        id: String,
        getValue: () => String,
        nodes: Seq[Node],
        isDynamic: Boolean
    ): FormFieldsState =
      val updatedInputs = state.activeInputs + (id -> getValue)
      val updatedDynamicNodes =
        if isDynamic then state.dynamicNodes ++ nodes else state.dynamicNodes
      state.copy(activeInputs = updatedInputs, dynamicNodes = updatedDynamicNodes)

    def withoutDependentFieldsOf(spec: SelectFieldSpec): FormFieldsState =
      val idsToRemove = spec.dependentFields.values.flatten.map(_.id).toSet
      state.copy(
        activeInputs = state.activeInputs.removedAll(idsToRemove),
        dynamicNodes = Seq.empty
      )

    def currentValues: Map[String, String] =
      state.activeInputs.map { case (id, readVal) => id -> readVal() }
