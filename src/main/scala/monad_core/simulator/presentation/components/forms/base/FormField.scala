package monad_core.simulator.presentation.components.forms.base

/** Declarative description of a field rendered by a form dialog. */
sealed trait FormFieldSpec {

  /** Stable key used to identify the field in submitted values. */
  def id: String

  /** Human-readable label displayed next to the field. */
  def label: String

  /** Initial textual representation, when one is available. */
  def defaultValue: Option[String]
}

/**
 * Specification of a free-text input.
 *
 * @param id
 *   key used in submitted values
 * @param label
 *   text displayed next to the input
 * @param defaultValue
 *   initial text, or `None` for an empty input
 */
final case class TextFieldSpec(
    id: String,
    label: String,
    defaultValue: Option[String] = None
) extends FormFieldSpec

/**
 * Specification of a single-choice input.
 *
 * @param id
 *   key used in submitted values
 * @param label
 *   text displayed next to the input
 * @param options
 *   selectable textual values
 * @param defaultValue
 *   initially selected value; the first option is used when absent
 * @param dependentFields
 *   additional fields to display for each selected value
 */
final case class SelectFieldSpec(
    id: String,
    label: String,
    options: Seq[String],
    defaultValue: Option[String] = None,
    dependentFields: Map[String, Seq[FormFieldSpec]] = Map.empty
) extends FormFieldSpec

/**
 * Specification of a multiple-choice input.
 *
 * Selected values are submitted as a comma-separated string.
 *
 * @param id
 *   key used in submitted values
 * @param label
 *   text displayed next to the input
 * @param options
 *   selectable textual values
 * @param defaultValues
 *   values selected when the control is created
 */
final case class MultiSelectFieldSpec(
    id: String,
    label: String,
    options: Seq[String],
    defaultValues: Seq[String] = Seq.empty
) extends FormFieldSpec:

  override def defaultValue: Option[String] =
    Option.when(defaultValues.nonEmpty)(defaultValues.mkString(","))
