package integrations.monad_core.simulator.presentation.components.forms

import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import integrations.monad_core.simulator.presentation.support.{DialogTesting, FormTesting}
import monad_core.engine.model.*
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.*
import org.scalamock.scalatest.proxy.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.Includes.*

class SaveSurfaceFormDialogTest
    extends AnyFunSuite
    with Inside
    with Matchers
    with MockFactory
    with DialogTesting
    with FormTesting:
  val RadiusFieldIndex: Int = 2

  val GenericEitherCircleSurface: Either[EngineError, Surface] =
    Surface.circle("id", Vector2D(0, 0), 6)

  private def selectShapeInComboBox(shapeIndex: Int): Unit =
    val ShapeComboBoxIndex = 0
    allFormComboBoxes(ShapeComboBoxIndex).getSelectionModel.select(shapeIndex)

  private def selectCircleInComboBox(): Unit =
    selectShapeInComboBox(0)

  private def selectRectangleInComboBox(): Unit =
    selectShapeInComboBox(1)

  private def buildCompleteSurface(eitherSurface: Either[EngineError, Surface]): Surface =
    val either = for
      surface                  <- eitherSurface
      surfaceWithFrictionIndex <- surface.withFrictionIndex(10)
      finalSurface             <- surfaceWithFrictionIndex.withAppliedForce(Vector2D(12, 13))
    yield finalSurface

    either.value

  test("SaveSurfaceFormDialog opens successfully"):
    val props = SaveSurfaceFormDialogProps(
      title = "Create Surface",
      onSubmit = _ => (),
      onError = _ => ()
    )

    onFxThread {
      val result = SaveSurfaceFormDialog.show(props)
      inside(result):
        case Right(_) => ()
    }

  test("SaveSurfaceFormDialog opens successfully by providing an Surface to edit"):
    val props = SaveSurfaceFormDialogProps(
      title = "Edit Surface",
      onSubmit = _ => (),
      onError = _ => (),
      surfaceToUpdate = Some(GenericEitherCircleSurface.value)
    )

    onFxThread {
      val result = SaveSurfaceFormDialog.show(props)
      inside(result):
        case Right(_) => ()
    }

  test("SaveSurfaceFormDialog invokes onSubmit with constructed Surface on valid input"):
    var submittedSurface: Option[Surface] = None

    val props = SaveSurfaceFormDialogProps(
      title = "Add Surface Test",
      onSubmit = Surface => submittedSurface = Some(Surface),
      onError = err => fail(s"Unexpected error: $err")
    )

    onFxThread {
      getOrFail(SaveSurfaceFormDialog.show(props))

      allFormFields(RadiusFieldIndex).setText("10.0")
      selectCircleInComboBox()

      formSaveButton.fire()
    }

    submittedSurface shouldBe defined

  test(
    "SaveSurfaceFormDialog invokes onSubmit with constructed Surface on valid input, with passed surfaceToUpdate"
  ):
    val surfaceToUpdate                   = buildCompleteSurface(GenericEitherCircleSurface)
    var submittedSurface: Option[Surface] = None
    val expectedRadius                    = 10.0
    val expectedWeight                    = 70.0
    val expectedHealth                    = 100.0

    val props = SaveSurfaceFormDialogProps(
      title = "Add Surface Test",
      onSubmit = Surface => submittedSurface = Some(Surface),
      onError = err => fail(s"Unexpected error: $err"),
      surfaceToUpdate = Some(surfaceToUpdate)
    )

    onFxThread {
      getOrFail(SaveSurfaceFormDialog.show(props))

      allFormFields(RadiusFieldIndex).setText(expectedRadius.toString)
      formSaveButton.fire()
    }

    submittedSurface shouldBe defined
    val providedSurface: Surface = submittedSurface.get
    providedSurface.id should be(surfaceToUpdate.id)
    providedSurface.position should be(surfaceToUpdate.position)

    inside(providedSurface.shape):
      case Shape2D.Circle(radius) => radius should be(expectedRadius)

  test("SaveSurfaceFormDialog invokes onError when form values are invalid"):
    var capturedError: Option[BaseError] = None

    val props = SaveSurfaceFormDialogProps(
      title = "Invalid Surface Test",
      onSubmit = _ => fail("onSubmit should not be called with invalid inputs"),
      onError = err => capturedError = Some(err)
    )

    onFxThread {
      getOrFail(SaveSurfaceFormDialog.show(props))

      formSaveButton.fire()
    }

    capturedError shouldBe defined

  test("SaveSurfaceFormDialog displays visually the circle Surface values passed"):
    val circleSurfaceToUpdate: Surface    = buildCompleteSurface(GenericEitherCircleSurface)
    var submittedSurface: Option[Surface] = Option.empty

    val props = SaveSurfaceFormDialogProps(
      title = "Edit Surface Test",
      onSubmit = Surface => submittedSurface = Some(Surface),
      onError = err => fail(s"Unexpected error: $err"),
      surfaceToUpdate = Some(circleSurfaceToUpdate)
    )

    onFxThread {
      getOrFail(SaveSurfaceFormDialog.show(props))

      val activeStage                  = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesVisualSnapshot(
        "edit_circle_surface_form_dialog",
        rootNode,
        maxDiffPercentage = 10.0
      )
    }

  test("SaveSurfaceFormDialog displays architecturally the circle Surface values passed"):
    val circleSurfaceToUpdate: Surface =
      buildCompleteSurface(Surface.circle("id", Vector2D(0, 0), 6))
    var submittedSurface: Option[Surface] = Option.empty

    val props = SaveSurfaceFormDialogProps(
      title = "Edit Surface Test",
      onSubmit = Surface => submittedSurface = Some(Surface),
      onError = err => fail(s"Unexpected error: $err"),
      surfaceToUpdate = Some(circleSurfaceToUpdate)
    )

    onFxThread {
      getOrFail(SaveSurfaceFormDialog.show(props))

      val activeStage                  = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesArchitecturalSnapshotOfStage("edit_circle_surface_form_dialog", activeStage)
    }

  test("SaveSurfaceFormDialog displays visually the rectangle Surface values passed"):
    val rectangleSurfaceToUpdate: Surface =
      buildCompleteSurface(Surface.rectangle("id", Vector2D(0, 0), 6, 10))
    var submittedSurface: Option[Surface] = Option.empty

    val props = SaveSurfaceFormDialogProps(
      title = "Edit Surface Test",
      onSubmit = Surface => submittedSurface = Some(Surface),
      onError = err => fail(s"Unexpected error: $err"),
      surfaceToUpdate = Some(rectangleSurfaceToUpdate)
    )

    onFxThread {
      getOrFail(SaveSurfaceFormDialog.show(props))

      val activeStage                  = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesVisualSnapshot(
        "edit_rectangle_surface_form_dialog",
        rootNode,
        maxDiffPercentage = 10.0
      )
    }

  test("SaveSurfaceFormDialog displays architecturally the rectangle Surface values passed"):
    val rectangleSurfaceToUpdate: Surface =
      buildCompleteSurface(Surface.rectangle("id", Vector2D(0, 0), 6, 10))
    var submittedSurface: Option[Surface] = Option.empty

    val props = SaveSurfaceFormDialogProps(
      title = "Edit Surface Test",
      onSubmit = Surface => submittedSurface = Some(Surface),
      onError = err => fail(s"Unexpected error: $err"),
      surfaceToUpdate = Some(rectangleSurfaceToUpdate)
    )

    onFxThread {
      getOrFail(SaveSurfaceFormDialog.show(props))

      val activeStage                  = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesArchitecturalSnapshotOfStage("edit_rectangle_surface_form_dialog", activeStage)
    }

  test("SaveSurfaceFormDialog Circle matches visual snapshot"):
    val props = SaveSurfaceFormDialogProps(
      title = "Visual Save Surface Test",
      onSubmit = _ => (),
      onError = _ => ()
    )

    onFxThread {
      getOrFail(SaveSurfaceFormDialog.show(props))

      selectCircleInComboBox()

      val activeStage                  = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesVisualSnapshot(
        "save_circle_surface_form_dialog_initial",
        rootNode,
        maxDiffPercentage = 9.2
      )
    }

  test("SaveSurfaceFormDialog Circle matches architectural snapshot"):
    val props = SaveSurfaceFormDialogProps(
      title = "Visual Save Surface Test",
      onSubmit = _ => (),
      onError = _ => ()
    )

    onFxThread {
      getOrFail(SaveSurfaceFormDialog.show(props))

      selectCircleInComboBox()

      val activeStage                  = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesArchitecturalSnapshotOfStage(
        "save_circle_surface_form_dialog_initial",
        activeStage
      )
    }

  test("SaveSurfaceFormDialog Rectangle matches visual snapshot"):
    val props = SaveSurfaceFormDialogProps(
      title = "Visual Save Surface Test",
      onSubmit = _ => (),
      onError = _ => ()
    )

    onFxThread {
      getOrFail(SaveSurfaceFormDialog.show(props))

      selectRectangleInComboBox()

      val activeStage                  = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesVisualSnapshot(
        "save_rectangle_surface_form_dialog_initial",
        rootNode,
        maxDiffPercentage = 9.2
      )
    }

  test("SaveSurfaceFormDialog Rectangle matches architectural snapshot"):
    val props = SaveSurfaceFormDialogProps(
      title = "Visual Save Surface Test",
      onSubmit = _ => (),
      onError = _ => ()
    )

    onFxThread {
      getOrFail(SaveSurfaceFormDialog.show(props))

      selectRectangleInComboBox()

      val activeStage                  = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesArchitecturalSnapshotOfStage(
        "save_rectangle_surface_form_dialog_initial",
        activeStage
      )
    }
