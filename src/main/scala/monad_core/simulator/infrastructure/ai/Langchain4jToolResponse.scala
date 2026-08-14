package monad_core.simulator.infrastructure.ai

import monad_core.simulator.domain.engine.MonadCoreShape.{SimulationCircle, SimulationRectangle}
import monad_core.simulator.domain.engine.{
  MonadCoreEntity,
  MonadCoreShape,
  MonadCoreSurface,
  MonadCoreTeam
}
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

  def getSafeList[T](
      result: Either[BaseError, List[T]]
  )(onSuccess: List[T] => String): String =
    result match
      case Left(error) =>
        s"Error: ${error.message}"
      case Right(list) => onSuccess(list)

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

  def renderEntity(entity: MonadCoreEntity): String =
    List(
      s"id: ${entity.id}",
      s"position: ${renderVector(entity.position)}",
      s"shape: ${renderShape(entity.shape)}",
      s"speed: ${entity.speed.fold("none")(renderVector)}",
      s"weight: ${entity.weight.fold("none")(_.toString)}",
      s"health: ${entity.health.fold("none")(_.toString)}",
      // the following to string is required to get the actual value of the teamId.
      // if removed the class signature is inserted in the string.
      s"team: ${entity.teamId.fold("none")(_.toString)}"
    ).mkString("\n")

  def renderSurface(surface: MonadCoreSurface): String =
    List(
      s"id: ${surface.id}",
      s"position: ${renderVector((surface.position._1, surface.position._2))}",
      s"shape: ${renderShape(surface.shape)}",
      s"frictionIndex: ${surface.frictionIndex.fold("none")(_.toString)}",
      s"appliedForce: ${surface.appliedForce.fold("none")(renderVector)}"
    ).mkString("\n")

  def renderTeam(team: MonadCoreTeam): String =
    val enemies =
      if team.enemies.isEmpty then "none"
      else team.enemies.iterator.toList.sorted.mkString(", ")

    List(
      s"id: ${team.id}",
      s"enemies: $enemies"
    ).mkString("\n")

  private def renderShape(shape: MonadCoreShape): String =
    shape match
      case SimulationCircle(radius) =>
        s"circle, radius: $radius"
      case SimulationRectangle(width, height) =>
        s"rectangle, height: $height, length: $width"

  private def renderVector(vector: (Double, Double)): String =
    s"(${vector._1}, ${vector._2})"
