package monad_core.simulator.presentation.components.forms

import monad_core.engine.errors.EngineError
import monad_core.simulator.CannotBuildDialog
import scalafx.Includes.*
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.{Node, Scene}
import scalafx.scene.control.{Button, ComboBox, Label, TextField}
import scalafx.scene.layout.{GridPane, HBox, VBox}
import scalafx.stage.{Modality, Stage, Window}

import scala.collection.mutable

final case class FormDialogProps(
                                  title: String,
                                  fields: Seq[FormFieldSpec],
                                  onSubmit: Map[String, String] => Unit,
                                  owner: Option[Window] = None
                                )

object FormDialog {

  private val StylesheetPath: String =
    getClass.getResource("/stylesheets/form-dialog.css").toExternalForm

  private def readValue(input: javafx.scene.Node): String = input match
    case tf: javafx.scene.control.TextField => tf.getText
    case cb: javafx.scene.control.ComboBox[String] @unchecked =>
      Option(cb.getValue).getOrElse("")

  def show(props: FormDialogProps): Either[EngineError, Unit] =
    try
      val allInputs: mutable.LinkedHashMap[String, javafx.scene.Node] = mutable.LinkedHashMap.empty

      def buildTextInput(spec: TextFieldSpec): javafx.scene.Node =
        new TextField {
          text = spec.defaultValue.getOrElse("")
          styleClass += "form-field-input"
        }.delegate

      def buildSelectInput(spec: SelectFieldSpec, onSelected: String => Unit): ComboBox[String] =
        new ComboBox[String] {
          items = ObservableBuffer.from(spec.options)
          styleClass += "form-field-select"
          spec.defaultValue.orElse(spec.options.headOption).foreach(value = _)
          onAction = _ => onSelected(Option(value.value).getOrElse(""))
        }

      def labeledRow(spec: FormFieldSpec, input: javafx.scene.Node): HBox =
        new HBox {
          spacing = 12
          alignment = Pos.CenterLeft
          children = Seq(
            new Label(spec.label) { styleClass += "form-field-label" }: scalafx.scene.Node,
            jfxNode2sfx(input)
          )
        }

      val dynamicContainer = new VBox {
        spacing = 12
        padding = Insets(12, 20, 0, 20)
      }

      def renderDependentFields(spec: SelectFieldSpec, selectedValue: String): Unit =
        spec.dependentFields.values.flatten.map(_.id).foreach(allInputs.remove)
        dynamicContainer.children.clear()

        spec.dependentFields.getOrElse(selectedValue, Seq.empty).foreach {
          case s: TextFieldSpec =>
            val input = buildTextInput(s)
            allInputs(s.id) = input
            dynamicContainer.children.add(labeledRow(s, input))

          case s: SelectFieldSpec =>
            val nested = buildSelectInput(s, _ => ())
            allInputs(s.id) = nested.delegate
            dynamicContainer.children.add(labeledRow(s, nested.delegate))
        }

      val grid = new GridPane {
        hgap = 12
        vgap = 12
        padding = Insets(20, 20, 0, 20)
        styleClass += "form-dialog-grid"
      }
      
      def addInputToGrid(spec: FormFieldSpec, input: Node, rowIndex: Int) : Unit =
        grid.add(new Label(spec.label) { styleClass += "form-field-label" }, 0, rowIndex)
        grid.add(input, 1, rowIndex)

      props.fields.zipWithIndex.foreach {
        case (spec: TextFieldSpec, row) =>
          val input = buildTextInput(spec)
          allInputs(spec.id) = input
          addInputToGrid(spec, input, row)

        case (spec: SelectFieldSpec, row) if spec.dependentFields.nonEmpty =>
          val combo = buildSelectInput(spec, selected => renderDependentFields(spec, selected))
          allInputs(spec.id) = combo.delegate
          addInputToGrid(spec, combo, row)
          Option(combo.value.value).foreach(renderDependentFields(spec, _))

        case (spec: SelectFieldSpec, row) =>
          val combo = buildSelectInput(spec, _ => ())
          allInputs(spec.id) = combo.delegate
          addInputToGrid(spec, combo, row)
      }

      val stage = new Stage {
        title = props.title
        initModality(Modality.WindowModal)
        props.owner.foreach(initOwner)
      }

      val cancelBtn = new Button("Annulla") {
        styleClass += "form-dialog-cancel"
        onAction = _ => stage.close()
      }

      val saveBtn = new Button("Salva") {
        styleClass += "form-dialog-save"
        onAction = _ =>
          val values: Map[String, String] =
            allInputs.map { case (id, input) => id -> readValue(input) }.toMap
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
        children = Seq(grid, dynamicContainer, buttonsRow)
      }

      val scene = new Scene(root) {
        stylesheets += StylesheetPath
      }

      stage.scene = scene
      stage.show()

      Right(())
    catch
      case error: Exception =>
        Left(CannotBuildDialog(error.getMessage, FormDialog.toString))
}