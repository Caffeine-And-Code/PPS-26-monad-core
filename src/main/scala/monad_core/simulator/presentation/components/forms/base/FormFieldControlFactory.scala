package monad_core.simulator.presentation.components.forms.base

import monad_core.simulator.presentation.components.forms.*
import scalafx.Includes.*
import scalafx.collections.ObservableBuffer
import scalafx.scene.Node
import scalafx.scene.control.*

/** Interprets declarative field specifications as ScalaFX controls. */
private[forms] object FormFieldControlFactory:

  /**
   * Creates the control for a field and the function used to read its submitted value.
   *
   * For a selection with dependent fields, the callback is invoked for the initial selection and after each change.
   *
   * @param spec
   *   field specification to render
   * @param onDependentSelectionChange
   *   callback used to replace fields that depend on a selected option
   * @return
   *   the rendered node and a function that reads its current textual value
   */
  def create(
      spec: FormFieldSpec,
      onDependentSelectionChange: (SelectFieldSpec, String) => Unit
  ): (Node, () => String) =
    spec match
      case tf: TextFieldSpec =>
        val field = new TextField {
          text = tf.defaultValue.getOrElse("")
          styleClass += "form-field-input"
          id = spec.id
        }
        (field, () => field.text.value)

      case select: SelectFieldSpec =>
        val combo = new ComboBox[String] {
          items = ObservableBuffer.from(select.options)
          styleClass += "form-field-select"
          id = spec.id
          select.defaultValue.orElse(select.options.headOption).foreach(v => value = v)
        }

        if select.dependentFields.nonEmpty then
          combo.onAction = _ =>
            onDependentSelectionChange(select, Option(combo.value.value).getOrElse(""))
          Option(combo.value.value).foreach(v => onDependentSelectionChange(select, v))

        (combo, () => Option(combo.value.value).getOrElse(""))

      case multi: MultiSelectFieldSpec =>
        val listView = new ListView[String](ObservableBuffer.from(multi.options)) {
          styleClass += "form-field-multiselect"
          id = spec.id
          prefHeight = 120
          maxHeight = 120
          fixedCellSize = 24
        }
        listView.selectionModel.value.selectionMode = SelectionMode.Multiple

        multi.defaultValues.foreach { v =>
          val idx = multi.options.indexOf(v)
          if idx >= 0 then listView.selectionModel.value.select(idx)
        }

        val getValue: () => String = () =>
          listView.selectionModel.value.getSelectedItems.mkString(",")

        (listView, getValue)
