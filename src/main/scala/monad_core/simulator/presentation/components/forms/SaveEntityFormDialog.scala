package monad_core.simulator.presentation.components.forms

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, Team, Vector2D}
import monad_core.simulator.InvalidFormFieldError
import scalafx.stage.Window

import scala.util.Random

final case class SaveEntityFormDialogProps(
                                            title: String,
                                            onSubmit: Entity => Unit,
                                            teams: Seq[Team],
                                            owner: Option[Window] = None,
                                          )

object SaveEntityFormDialog:

  def show(props: SaveEntityFormDialogProps): Either[EngineError, Unit] = {
    val shapes = Seq("Cerchio", "Rettangolo")

    def generateEntityId(): String =
      Random.alphanumeric.take(10).mkString

    def requiredDouble(values: Map[String, String], key: String): Either[EngineError, Double] =
      values.get(key)
        .flatMap(_.toDoubleOption)
        .toRight(InvalidFormFieldError(key, "valore numerico richiesto"))

    def buildEntity(values: Map[String, String]): Either[EngineError, Entity] =
      val id = generateEntityId()

      for
        x <- requiredDouble(values, "x")
        y <- requiredDouble(values, "y")
        position = Vector2D(x, y)

        shapeValue <- values.get("shape")
          .toRight(InvalidFormFieldError("shape", "forma non selezionata"))

        entity <- shapeValue match
          case "Cerchio" =>
            for
              radius <- requiredDouble(values, "radius")
              entity <- Entity.circle(id, position, radius)
            yield entity

          case "Rettangolo" =>
            for
              height <- requiredDouble(values, "height")
              length <- requiredDouble(values, "length")
              entity <- Entity.rectangle(id, position, height, length)
            yield entity

          case other =>
            Left(InvalidFormFieldError("shape", s"forma sconosciuta: $other"))
      yield entity

    FormDialog.show(
      FormDialogProps(
        title = props.title,
        fields = Seq(
          TextFieldSpec(id = "x", label = "Posizione iniziale x", defaultValue = Option("10.0")),
          TextFieldSpec(id = "y", label = "Posizione iniziale y", defaultValue = Option("10.0")),
          SelectFieldSpec(
            id = "shape",
            label = "Forma Geometrica",
            options = shapes,
            dependentFields = Map(
              "Cerchio" -> Seq(
                TextFieldSpec(id = "radius", label = "Raggio")
              ),
              "Rettangolo" -> Seq(
                TextFieldSpec(id = "height", label = "Altezza"),
                TextFieldSpec(id = "length", label = "Lunghezza")
              )
            )
          ),
          TextFieldSpec(id = "speed", label = "Velocità"),
          TextFieldSpec(id = "weight", label = "Peso"),
          TextFieldSpec(id = "health", label = "Vita"),
          SelectFieldSpec(
            id = "teamId",
            label = "Team di Appartenenza",
            options = props.teams.map(team => team.id.value)
          )
        ),
        owner = props.owner,
        onSubmit = values =>
          buildEntity(values) match
            case Right(entity) => props.onSubmit(entity)
            case Left(error) => println(s"Errore nella creazione dell'entità: $error")
      )
    )
  }
