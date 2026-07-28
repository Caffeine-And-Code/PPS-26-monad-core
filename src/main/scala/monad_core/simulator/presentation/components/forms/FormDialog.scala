package monad_core.simulator.presentation.components.forms

import monad_core.engine.errors.EngineError
import monad_core.simulator.CannotBuildDialog
import scalafx.Includes.*
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{Button, ComboBox, Label, ListView, SelectionMode, TextField}
import scalafx.scene.layout.{GridPane, HBox, VBox}
import scalafx.scene.{Node, Scene}
import scalafx.stage.{Modality, Stage, Window}

import scala.util.Try

final case class FormDialogProps(
                                  title: String,
                                  fields: Seq[FormFieldSpec],
                                  onSubmit: Map[String, String] => Unit,
                                  owner: Option[Window] = None,
                                  minWidth: Double = 500
                                )

object FormDialog:

  private[forms] val StylesheetPath: String =
    getClass.getResource("/stylesheets/form-dialog.css").toExternalForm

  def show(props: FormDialogProps): Either[EngineError, Unit] =
    Try {
      val builder = new FormDialogBuilder(props)
      builder.display()
    }.toEither.left.map(ex => CannotBuildDialog(ex.getMessage, "FormDialog"))

private final class FormDialogBuilder(props: FormDialogProps):

  private var activeInputs: Map[String, () => String] = Map.empty
  private var dynamicNodes: Seq[Node] = Seq.empty

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

    val saveBtn = new Button("Save") {
      styleClass += "form-dialog-save"
      onAction = _ =>
        val values = activeInputs.map { case (id, readVal) => id -> readVal() }
        props.onSubmit(values)
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
    val (controlNode, getValue) = createControl(spec)

    activeInputs = activeInputs + (spec.id -> getValue)
    grid.add(label, 0, row)
    grid.add(controlNode, 1, row)

    if isDynamic then
      dynamicNodes = dynamicNodes :+ label :+ controlNode

  private def createControl(spec: FormFieldSpec): (Node, () => String) =
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
          combo.onAction = _ => updateDependentFields(select, Option(combo.value.value).getOrElse(""))
          Option(combo.value.value).foreach(v => updateDependentFields(select, v))

        (combo, () => Option(combo.value.value).getOrElse(""))

      case multi: MultiSelectFieldSpec =>
        val listView = new ListView[String](ObservableBuffer.from(multi.options)) {
          styleClass += "form-field-multiselect"
          id = spec.id
          prefHeight = 120
          maxHeight = 120
        }
        listView.selectionModel.value.selectionMode = SelectionMode.Multiple

        multi.defaultValues.foreach { v =>
          val idx = multi.options.indexOf(v)
          if idx >= 0 then listView.selectionModel.value.select(idx)
        }

        val getValue: () => String = () =>
          listView.selectionModel.value.getSelectedItems.mkString(",")

        (listView, getValue)

  private def updateDependentFields(spec: SelectFieldSpec, selectedValue: String): Unit =
    val idsToRemove = spec.dependentFields.values.flatten.map(_.id).toSet
    activeInputs = activeInputs.removedAll(idsToRemove)

    dynamicNodes.foreach(node => grid.children.remove(node))
    dynamicNodes = Seq.empty

    val dependentSpecs = spec.dependentFields.getOrElse(selectedValue, Seq.empty)
    val startRow = props.fields.length

    dependentSpecs.zipWithIndex.foreach { (depSpec, idx) =>
      renderField(depSpec, startRow + idx, isDynamic = true)
    }

    stage.sizeToScene()