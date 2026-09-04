package monad_core.simulator.infrastructure.ai

import monad_core.engine.model.*
import monad_core.simulator.errors.BaseError

/** Formats world operations as stable plain text responses for the assistant. */
object Langchain4jToolResponse:

  /**
   * Convert in plain text a save operation result or an error.
   *
   * @param result operation result
   * @param successMessage description used on success @return formatted success or error response
   */
  def save(
      result: Either[BaseError, Unit],
      successMessage: String
  ): String =
    result match
      case Left(error) =>
        s"Error: ${error.message}"
      case Right(_) =>
        s"Success: $successMessage"

  /**
   * Convert in plain text a value of type `A` or an error.
   *
   * @tparam A successful value type
   * @param result operation result
   * @param format successful value renderer
   * @return formatted result or error response
   */
  def render[A](
      result: Either[BaseError, A]
  )(
      format: A => String
  ): String =
    result.fold(
      error => s"Error: ${error.message}",
      value => s"Result:\n${format(value)}"
    )

  /**
   * Convert in plain text a list of values.
   *
   * @tparam A element type
   * @param values elements to render
   * @param elementName plural display name
   * @param format element renderer
   * @return plain text list
   */
  def renderList[A](
      values: List[A],
      elementName: String
  )(
      format: A => String
  ): String =
    if values.isEmpty then s"Result: no $elementName found."
    else
      val renderedValues = values.zipWithIndex
        .map((value, index) => s"${index + 1}:\n${format(value)}")
        .mkString("\n\n")

      s"Result: ${values.size} $elementName found.\n$renderedValues"

  /**
   * Convert in plain text an [[Entity]].
   *
   * @param entity entity to render
   * @return plain text entity description
   */
  def renderEntity(entity: Entity): String =
    List(
      s"id: ${entity.id.value}",
      s"position: ${renderVector(entity.position)}",
      s"shape: ${renderShape(entity.shape)}",
      s"rotation: ${entity.rotation}",
      s"speed: ${entity.speed.fold("none")(renderVector)}",
      s"angularSpeed: ${entity.angularSpeed.fold("none")(_.toString)}",
      s"weight: ${entity.weight.fold("none")(_.toString)}",
      s"health: ${entity.health.fold("none")(_.value.toString)}",
      s"damage: ${entity.damage.fold("none")(_.value.toString)}",
      s"team: ${entity.teamId.fold("none")(_.value)}"
    ).mkString("\n")

  /**
   * Convert in plain text a [[Surface]]
   *
   * @param surface surface to render
   * @return plain text surface description
   */
  def renderSurface(surface: Surface): String =
    List(
      s"id: ${surface.id.value}",
      s"position: ${renderVector(surface.position)}",
      s"shape: ${renderShape(surface.shape)}",
      s"rotation: ${surface.rotation}",
      s"frictionIndex: ${surface.frictionIndex.fold("none")(_.toString)}",
      s"appliedForce: ${surface.appliedForce.fold("none")(renderVector)}",
      s"damageOverTime: ${surface.damageOverTime.fold("none")(_.value.toString)}"
    ).mkString("\n")

  /**
   * Convert in plain text a [[Team]]
   *
   * @param team team to render
   * @return plain text team description
   */
  def renderTeam(team: Team): String =
    val enemies =
      if team.enemies.isEmpty then "none"
      else team.enemies.iterator.map(_.value).toList.sorted.mkString(", ")

    List(
      s"id: ${team.id.value}",
      s"enemies: $enemies"
    ).mkString("\n")

  private def renderShape(shape: Shape2D): String =
    shape match
      case Shape2D.Circle(radius) =>
        s"circle, radius: $radius"
      case Shape2D.Rectangle(height, length) =>
        s"rectangle, height: $height, length: $length"

  private def renderVector(vector: Vector2D): String =
    s"(${vector.x}, ${vector.y})"
