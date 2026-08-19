package monad_core.simulator.presentation.components.forms.base

sealed trait FormFieldSpec {
  def id: String

  def label: String

  def defaultValue: Option[String]
}

final case class TextFieldSpec(
    id: String,
    label: String,
    defaultValue: Option[String] = None
) extends FormFieldSpec

final case class SelectFieldSpec(
    id: String,
    label: String,
    options: Seq[String],
    defaultValue: Option[String] = None,
    dependentFields: Map[String, Seq[FormFieldSpec]] = Map.empty
) extends FormFieldSpec

final case class MultiSelectFieldSpec(
    id: String,
    label: String,
    options: Seq[String],
    defaultValues: Seq[String] = Seq.empty
) extends FormFieldSpec:

  override def defaultValue: Option[String] =
    Option.when(defaultValues.nonEmpty)(defaultValues.mkString(","))
