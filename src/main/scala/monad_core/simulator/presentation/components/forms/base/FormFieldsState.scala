package monad_core.simulator.presentation.components.forms.base

import scalafx.scene.Node

/**
 * Immutable registry of active form readers and dynamically rendered nodes.
 *
 * @param activeInputs
 *   field identifiers associated with functions that read their current value
 * @param dynamicNodes
 *   nodes created for fields that depend on another selection
 */
final private[forms] case class FormFieldsState(
    activeInputs: Map[String, () => String] = Map.empty,
    dynamicNodes: Seq[Node] = Seq.empty
)

/** State transitions used while a form dialog is being rendered. */
private[forms] object FormFieldsState:

  extension (state: FormFieldsState)

    /**
     * Registers a field reader and, for a dependent field, its rendered nodes.
     *
     * @param id
     *   identifier of the field
     * @param getValue
     *   function that reads the current field value
     * @param nodes
     *   label and control associated with the field
     * @param isDynamic
     *   whether the nodes must be removed when their parent selection changes
     * @return
     *   updated immutable form state
     */
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

    /**
     * Removes every registered field that may depend on the supplied selection.
     *
     * @param spec
     *   selection whose dependent field identifiers must be removed
     * @return
     *   state without dependent readers or dynamic nodes
     */
    def withoutDependentFieldsOf(spec: SelectFieldSpec): FormFieldsState =
      val idsToRemove = spec.dependentFields.values.flatten.map(_.id).toSet
      state.copy(
        activeInputs = state.activeInputs.removedAll(idsToRemove),
        dynamicNodes = Seq.empty
      )

    /**
     * Evaluates all active readers.
     *
     * @return
     *   current form values indexed by field identifier
     */
    def currentValues: Map[String, String] =
      state.activeInputs.map { case (id, readVal) => id -> readVal() }
