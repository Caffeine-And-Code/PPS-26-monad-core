package monad_core.simulator.presentation.components.forms

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FormFieldSpecTest extends AnyFunSuite with Matchers:

  test("MultiSelectFieldSpec.defaultValue should be None when defaultValues is empty"):
    val spec = MultiSelectFieldSpec(id = "tags", label = "Tags", options = Seq("A", "B"))

    spec.defaultValue should be(None)

  test("MultiSelectFieldSpec.defaultValue should join defaultValues with a comma when non-empty"):
    val spec = MultiSelectFieldSpec(id = "tags", label = "Tags", options = Seq("A", "B", "C"), defaultValues = Seq("A", "C"))

    spec.defaultValue should be(Some("A,C"))

  test("MultiSelectFieldSpec.defaultValue should preserve defaultValues order when joining"):
    val spec = MultiSelectFieldSpec(id = "tags", label = "Tags", options = Seq("A", "B", "C"), defaultValues = Seq("C", "A"))

    spec.defaultValue should be(Some("C,A"))

  test("MultiSelectFieldSpec.defaultValue should handle a single default value without a comma"):
    val spec = MultiSelectFieldSpec(id = "tags", label = "Tags", options = Seq("A", "B"), defaultValues = Seq("A"))

    spec.defaultValue should be(Some("A"))