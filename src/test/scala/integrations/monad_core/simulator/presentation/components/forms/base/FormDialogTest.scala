package integrations.monad_core.simulator.presentation.components.forms.base

import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import integrations.monad_core.simulator.presentation.support.{DialogTesting, FormTesting}
import monad_core.simulator.presentation.components.forms.base.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table
import scalafx.Includes.*
import scalafx.scene.control.{Button, ComboBox}

class FormDialogTest
    extends AnyFunSuite
    with Inside
    with Matchers
    with MockFactory
    with DialogTesting
    with FormTesting:

  private val defaultFields: Seq[FormFieldSpec] = Seq(
    TextFieldSpec(id = "name", label = "Name", defaultValue = Some("Entity_1")),
    SelectFieldSpec(
      id = "shape",
      label = "Shape",
      options = Seq("Circle", "Rectangle"),
      defaultValue = Some("Circle"),
      dependentFields = FormDialog.buildShapeFields(
        radiusDefaultValue = Some("5.0"),
        widthDefaultValue = Some("10.0"),
        heightDefaultValue = Some("20.0")
      )
    )
  )

  test("FormDialog can be built and shown successfully"):
    val onSubmitMock = mockFunction[Map[String, String], Unit]
    val props = FormDialogProps(
      title = "Test Dialog",
      fields = defaultFields,
      onSubmit = onSubmitMock
    )

    onFxThread {
      val buildResult = FormDialog.show(props)

      inside(buildResult):
        case Right(_) => ()
    }

  test("FormDialog onSubmit function is called upon submitting the form"):
    var submittedValues: Map[String, String] = Map.empty

    val props = FormDialogProps(
      title = "Submit Test",
      fields = defaultFields,
      onSubmit = values => submittedValues = values
    )

    onFxThread {
      getOrFail(FormDialog.show(props))
    }

    var saveButtonNode: javafx.scene.control.Button = null
    onFxThread {
      saveButtonNode = formSaveButton
    }

    clickButton(new Button(saveButtonNode))

    submittedValues shouldBe Map(
      "name"   -> "Entity_1",
      "shape"  -> "Circle",
      "radius" -> "5.0"
    )

  test("FormDialog updates dependent fields dynamically when changing select option"):
    var submittedValues: Map[String, String] = Map.empty
    val props = FormDialogProps(
      title = "Dynamic Fields Test",
      fields = defaultFields,
      onSubmit = values => submittedValues = values
    )

    onFxThread {
      getOrFail(FormDialog.show(props))

      val comboNode = allFormComboBoxes.head

      val comboBox = new ComboBox[String](comboNode)
      comboBox.selectionModel().select("Rectangle")

      formSaveButton.fire()
    }

    submittedValues shouldBe Map(
      "name"   -> "Entity_1",
      "shape"  -> "Rectangle",
      "length" -> "10.0",
      "height" -> "20.0"
    )

  test("FormDialog matches structural JSON snapshot of submitted values"):
    var capturedJsonString = ""
    val props = FormDialogProps(
      title = "Snapshot Test",
      fields = defaultFields,
      onSubmit = values =>
        capturedJsonString = values
          .map { case (k, v) => s""""$k": "$v"""" }
          .mkString("{\n  ", ",\n  ", "\n}")
    )

    onFxThread {
      getOrFail(FormDialog.show(props))

      val activeStage = getRequiredActiveStage

      formSaveButton.fire()
    }

    assertMatchesArchitecturalSnapshot("form_dialog_submitted_values", capturedJsonString)

  test("FormDialog matches visual snapshot of rendered dialog scene"):
    val props = FormDialogProps(
      title = "Visual Test",
      fields = defaultFields,
      onSubmit = _ => ()
    )

    onFxThread {
      getOrFail(FormDialog.show(props))

      val activeStage                  = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesVisualSnapshot("generic_form_dialog", rootNode, maxDiffPercentage = 8.0)
    }
