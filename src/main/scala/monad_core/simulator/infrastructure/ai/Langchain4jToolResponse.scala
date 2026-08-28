package monad_core.simulator.infrastructure.ai

import monad_core.engine.model.*
import monad_core.simulator.errors.BaseError

object Langchain4jToolResponse:

  def save(
      result: Either[BaseError, Unit],
      successMessage: String
  ): String =
    result match
      case Left(error) =>
        s"Error: ${error.message}"
      case Right(_) =>
        s"Success: $successMessage"

  def render[A](
      result: Either[BaseError, A]
  )(
      format: A => String
  ): String =
    result.fold(
      error => s"Error: ${error.message}",
      value => s"Result:\n${format(value)}"
    )

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
