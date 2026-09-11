package monad_core.simulator.presentation.performance

import monad_core.simulator.domain.performance.InvalidPerformanceArgument
import monad_core.simulator.application.performance.PerformanceExecutor
import monad_core.simulator.presentation.components.forms.base.{
  FormFieldSpec,
  SelectFieldSpec,
  TextFieldSpec
}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ExperimentFormTest extends AnyFunSuite with Matchers:

  private val KindField = ExperimentForm.fields.head.asInstanceOf[SelectFieldSpec]

  private val CommonValues = Map(
    KindField.id                          -> "Load",
    PerformanceExecutor.Entities          -> "10",
    PerformanceExecutor.Iterations        -> "3",
    PerformanceExecutor.Warmups           -> "1",
    PerformanceExecutor.FrameBudgetMillis -> "16",
    PerformanceExecutor.MaximumEntities   -> "100",
    PerformanceExecutor.GrowthFactor      -> "2"
  )

  private def commandFor(kind: String): ExperimentCommand =
    ExperimentForm.command(CommonValues.updated(KindField.id, kind)).value

  private def fieldIds(fields: Seq[FormFieldSpec]): Seq[String] =
    fields.collect { case field: TextFieldSpec => field.id }

  test("fields start with the test-type selector"):
    val result = ExperimentForm.fields.head

    result shouldBe KindField

  test("the test-type selector starts from stress"):
    val result = KindField.defaultValue

    result shouldBe Some("Stress")

  test("the test-type selector offers load"):
    val result = KindField.options

    result should contain("Load")

  test("the test-type selector offers stress"):
    val result = KindField.options

    result should contain("Stress")

  test("the test-type selector offers spike"):
    val result = KindField.options

    result should contain("Spike")

  test("the test-type selector offers scalability"):
    val result = KindField.options

    result should contain("Scalability")

  test("fields include the starting entity count"):
    val result = fieldIds(ExperimentForm.fields)

    result should contain(PerformanceExecutor.Entities)

  test("fields include the iteration count"):
    val result = fieldIds(ExperimentForm.fields)

    result should contain(PerformanceExecutor.Iterations)

  test("fields include the warm-up count"):
    val result = fieldIds(ExperimentForm.fields)

    result should contain(PerformanceExecutor.Warmups)

  test("fields include the frame budget"):
    val result = fieldIds(ExperimentForm.fields)

    result should contain(PerformanceExecutor.FrameBudgetMillis)

  test("load has no specific fields"):
    val result = KindField.dependentFields("Load")

    result shouldBe empty

  test("stress includes the maximum entity count"):
    val result = fieldIds(KindField.dependentFields("Stress"))

    result should contain(PerformanceExecutor.MaximumEntities)

  test("stress includes the growth factor"):
    val result = fieldIds(KindField.dependentFields("Stress"))

    result should contain(PerformanceExecutor.GrowthFactor)

  test("spike includes the maximum entity count"):
    val result = fieldIds(KindField.dependentFields("Spike"))

    result should contain(PerformanceExecutor.MaximumEntities)

  test("spike excludes the growth factor"):
    val result = fieldIds(KindField.dependentFields("Spike"))

    result should not contain PerformanceExecutor.GrowthFactor

  test("scalability includes the maximum entity count"):
    val result = fieldIds(KindField.dependentFields("Scalability"))

    result should contain(PerformanceExecutor.MaximumEntities)

  test("scalability includes the growth factor"):
    val result = fieldIds(KindField.dependentFields("Scalability"))

    result should contain(PerformanceExecutor.GrowthFactor)

  test("command maps load to its route"):
    val result = commandFor("Load")

    result.route shouldBe PerformanceExecutor.LoadRoute

  test("command maps stress to its route"):
    val result = commandFor("Stress")

    result.route shouldBe PerformanceExecutor.StressRoute

  test("command maps spike to its route"):
    val result = commandFor("Spike")

    result.route shouldBe PerformanceExecutor.SpikeRoute

  test("command maps scalability to its route"):
    val result = commandFor("Scalability")

    result.route shouldBe PerformanceExecutor.ScalabilityRoute

  test("command includes the starting entity count"):
    val result = commandFor("Load")

    result.arguments should contain(PerformanceExecutor.Entities)

  test("command includes the iteration count"):
    val result = commandFor("Load")

    result.arguments should contain(PerformanceExecutor.Iterations)

  test("command includes the warm-up count"):
    val result = commandFor("Load")

    result.arguments should contain(PerformanceExecutor.Warmups)

  test("command includes the frame budget"):
    val result = commandFor("Load")

    result.arguments should contain(PerformanceExecutor.FrameBudgetMillis)

  test("stress command includes the maximum entity count"):
    val result = commandFor("Stress")

    result.arguments should contain(PerformanceExecutor.MaximumEntities)

  test("stress command includes the growth factor"):
    val result = commandFor("Stress")

    result.arguments should contain(PerformanceExecutor.GrowthFactor)

  test("spike command excludes the growth factor"):
    val result = commandFor("Spike")

    result.arguments should not contain PerformanceExecutor.GrowthFactor

  test("command omits a missing optional value"):
    val values = CommonValues - PerformanceExecutor.Iterations

    val result = ExperimentForm.command(values).value

    result.arguments should not contain PerformanceExecutor.Iterations

  test("command rejects a missing test type"):
    val values = CommonValues - KindField.id

    val result = ExperimentForm.command(values)

    result shouldBe Left(InvalidPerformanceArgument(KindField.id, ""))

  test("command rejects an unknown test type"):
    val values = CommonValues.updated(KindField.id, "Unknown")

    val result = ExperimentForm.command(values)

    result shouldBe Left(InvalidPerformanceArgument(KindField.id, "Unknown"))
