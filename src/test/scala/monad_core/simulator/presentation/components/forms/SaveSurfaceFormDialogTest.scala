package monad_core.simulator.presentation.components.forms

import monad_core.engine.model.*
import monad_core.simulator.domain.engine.MonadCoreShape.{SimulationCircle, SimulationRectangle}
import monad_core.simulator.domain.engine.MonadCoreSurface
import monad_core.simulator.presentation.components.forms.base.{SelectFieldSpec, TextFieldSpec}
import monad_core.simulator.presentation.components.forms.parsers.{BaseFormParser, LocatableFormShapes, SurfaceFormParser}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class SaveSurfaceFormDialogTest extends AnyFunSuite with Inside with Matchers:

  private val Id: String = "id"
  private val Position: (Double, Double) = (1, 2)
  private val Circle: SimulationCircle = SimulationCircle(5)
  private val Rectangle: SimulationRectangle = SimulationRectangle(7.0, 6.0)
  private val AppliedForce: (Double, Double) = (3, 4)
  private val FrictionIndex: Double = 0.8

  private def circleSurface: MonadCoreSurface = MonadCoreSurface(Id, Position, Circle)

  private def rectangleSurface: MonadCoreSurface = MonadCoreSurface(Id, Position, Rectangle)

  private def completeSurface(surface: MonadCoreSurface): MonadCoreSurface =
    MonadCoreSurface(
      surface.id,
      surface.position,
      surface.shape,
      appliedForce = Some(AppliedForce),
      frictionIndex = Some(FrictionIndex)
    )

  test("buildDefaultValues should return the default creation values when no surface is provided"):
    val result = SaveSurfaceFormDialog.buildDefaultValues(None)

    result should be(SaveSurfaceFormDefaultValues())

  test("buildDefaultValues should map a circle surface's basic fields"):
    val surface = circleSurface

    val result = SaveSurfaceFormDialog.buildDefaultValues(Some(surface))

    result.x should be(Some(Position._1.toString))
    result.y should be(Some(Position._2.toString))
    result.shape should be(Some(LocatableFormShapes.CircleLabel))
    result.radius should be(Some(Circle.radius.toString))
    result.height should be(None)
    result.length should be(None)

  test("buildDefaultValues should map a rectangle surface's basic fields"):
    val surface = rectangleSurface

    val result = SaveSurfaceFormDialog.buildDefaultValues(Some(surface))

    result.x should be(Some(Position._1.toString))
    result.y should be(Some(Position._2.toString))
    result.shape should be(Some(LocatableFormShapes.RectangleLabel))
    result.radius should be(None)
    result.height should be(Some(Rectangle.height.toString))
    result.length should be(Some(Rectangle.width.toString))

  test("buildDefaultValues should leave optional fields empty when surface doesn't have them set"):
    val surface = circleSurface

    val result = SaveSurfaceFormDialog.buildDefaultValues(Some(surface))

    result.frictionIndex should be(None)
    result.appliedForceX should be(None)
    result.appliedForceY should be(None)

  test("buildDefaultValues should map optional fields when surface has them set"):
    val cases = Table(
      "surface",
      circleSurface,
      rectangleSurface
    )

    forAll(cases): baseSurface =>
      val surface = completeSurface(baseSurface)

      val result = SaveSurfaceFormDialog.buildDefaultValues(Some(surface))

      result.frictionIndex should be(Some(FrictionIndex.toString))
      result.appliedForceX should be(Some(AppliedForce._1.toString))
      result.appliedForceY should be(Some(AppliedForce._2.toString))


  test("buildFields should build all fields with correct ids"):
    val defaultValues = SaveSurfaceFormDefaultValues()

    val fields = SaveSurfaceFormDialog.buildFields(defaultValues)

    fields.map(_.id) should be(
      Seq(
        SurfaceFormParser.PositionXKey,
        SurfaceFormParser.PositionYKey,
        SurfaceFormParser.ShapeKey,
        SurfaceFormParser.AppliedForceXKey,
        SurfaceFormParser.AppliedForceYKey,
        SurfaceFormParser.FrictionIndexKey
      )
    )

  test("buildFields should propagate default values into the corresponding text fields"):
    val defaultValues = SaveSurfaceFormDefaultValues(
      x = Some("1.0"),
      y = Some("2.0"),
      appliedForceX = Some("3.0"),
      appliedForceY = Some("4.0"),
      frictionIndex = Some("0.5")
    )

    val fields = SaveSurfaceFormDialog.buildFields(defaultValues)

    inside(fields.find(_.id == SurfaceFormParser.PositionXKey).value):
      case tf: TextFieldSpec => tf.defaultValue should be(Some("1.0"))

    inside(fields.find(_.id == SurfaceFormParser.PositionYKey).value):
      case tf: TextFieldSpec => tf.defaultValue should be(Some("2.0"))

    inside(fields.find(_.id == SurfaceFormParser.AppliedForceXKey).value):
      case tf: TextFieldSpec => tf.defaultValue should be(Some("3.0"))

    inside(fields.find(_.id == SurfaceFormParser.AppliedForceYKey).value):
      case tf: TextFieldSpec => tf.defaultValue should be(Some("4.0"))

    inside(fields.find(_.id == SurfaceFormParser.FrictionIndexKey).value):
      case tf: TextFieldSpec => tf.defaultValue should be(Some("0.5"))

  test("buildFields should build the shape field with circle and rectangle dependent fields"):
    val defaultValues = SaveSurfaceFormDefaultValues()

    val fields = SaveSurfaceFormDialog.buildFields(defaultValues)

    inside(fields.find(_.id == SurfaceFormParser.ShapeKey).value):
      case select: SelectFieldSpec =>
        select.options should be(SaveSurfaceFormDialog.Shapes)

        val circleFields = select.dependentFields(LocatableFormShapes.CircleLabel)
        circleFields.map(_.id) should be(Seq(BaseFormParser.RadiusKey))

        val rectangleFields = select.dependentFields(LocatableFormShapes.RectangleLabel)
        rectangleFields.map(_.id) should be(Seq(BaseFormParser.HeightKey, BaseFormParser.LengthKey))

  test("buildFields should propagate shape-specific default values into dependent fields"):
    val defaultValues = SaveSurfaceFormDefaultValues(
      shape = Some(LocatableFormShapes.RectangleLabel),
      radius = Some("5.0"),
      height = Some("6.0"),
      length = Some("7.0")
    )

    val fields = SaveSurfaceFormDialog.buildFields(defaultValues)

    inside(fields.find(_.id == SurfaceFormParser.ShapeKey).value):
      case select: SelectFieldSpec =>
        select.defaultValue should be(Some(LocatableFormShapes.RectangleLabel))

        inside(select.dependentFields(LocatableFormShapes.CircleLabel).head):
          case tf: TextFieldSpec => tf.defaultValue should be(Some("5.0"))

        inside(select.dependentFields(LocatableFormShapes.RectangleLabel).find(_.id == BaseFormParser.HeightKey).value):
          case tf: TextFieldSpec => tf.defaultValue should be(Some("6.0"))

        inside(select.dependentFields(LocatableFormShapes.RectangleLabel).find(_.id == BaseFormParser.LengthKey).value):
          case tf: TextFieldSpec => tf.defaultValue should be(Some("7.0"))

  test("buildFields should default the shape select to Circle when creating a new surface"):
    val defaultValues = SaveSurfaceFormDefaultValues()

    val fields = SaveSurfaceFormDialog.buildFields(defaultValues)

    inside(fields.find(_.id == SurfaceFormParser.ShapeKey).value):
      case select: SelectFieldSpec => select.defaultValue should be(Some(LocatableFormShapes.CircleLabel))