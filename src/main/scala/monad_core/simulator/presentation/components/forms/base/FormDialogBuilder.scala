package monad_core.simulator.presentation.components.forms.base

import monad_core.simulator.presentation.components.forms.*
import monad_core.simulator.presentation.components.forms.base.FormFieldsState.*
import scalafx.Includes.*
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.*
import scalafx.scene.layout.{GridPane, HBox, VBox}
import scalafx.scene.Scene
import scalafx.stage.{Modality, Stage}

final private[forms] class FormDialogBuilder(props: FormDialogProps):

  private var fieldsState: FormFieldsState = FormFieldsState()

  private val grid = new GridPane {
    hgap = 12
    vgap = 12
    padding = Insets(20, 20, 12, 20)
    styleClass += "form-dialog-grid"
  }

  private val stage = new Stage {
    title = props.title
    resizable = false
    initModality(Modality.WindowModal)
    props.owner.foreach(initOwner)
  }

  def display(): Unit =
    props.fields.zipWithIndex.foreach { (spec, row) =>
      renderField(spec, row, isDynamic = false)
    }

    val cancelBtn = new Button("Cancel") {
      styleClass += "form-dialog-cancel"
      onAction = _ => stage.close()
    }

    val saveBtn = new Button(props.submitLabel) {
      styleClass += "form-dialog-save"
      onAction = _ =>
        props.onSubmit(fieldsState.currentValues)
        stage.close()
    }

    val buttonsRow = new HBox {
      spacing = 8
      alignment = Pos.CenterRight
      padding = Insets(12, 20, 20, 20)
      children = Seq(cancelBtn, saveBtn)
    }

    val root = new VBox {
      styleClass += "form-dialog-root"
      minWidth = props.minWidth
      prefWidth = props.minWidth
      children = Seq(grid, buttonsRow)
    }

    stage.scene = new Scene(root) {
      stylesheets += FormDialog.StylesheetPath
    }

    stage.sizeToScene()
    stage.show()

  private def renderField(spec: FormFieldSpec, row: Int, isDynamic: Boolean): Unit =
    val label = new Label(spec.label) {
      styleClass += "form-field-label"
    }
    val (controlNode, getValue) = FormFieldControlFactory.create(spec, updateDependentFields)

    grid.add(label, 0, row)
    grid.add(controlNode, 1, row)

    fieldsState = fieldsState.withField(spec.id, getValue, Seq(label, controlNode), isDynamic)

  private def updateDependentFields(spec: SelectFieldSpec, selectedValue: String): Unit =
    fieldsState.dynamicNodes.foreach(node => grid.children.remove(node))
    fieldsState = fieldsState.withoutDependentFieldsOf(spec)

    val dependentSpecs = spec.dependentFields.getOrElse(selectedValue, Seq.empty)
    val startRow       = props.fields.length

    dependentSpecs.zipWithIndex.foreach { (depSpec, idx) =>
      renderField(depSpec, startRow + idx, isDynamic = true)
    }

    stage.sizeToScene()
