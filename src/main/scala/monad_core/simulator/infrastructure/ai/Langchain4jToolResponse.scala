package monad_core.simulator.infrastructure.ai

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, Shape2D, Surface, Team, Vector2D}

object Langchain4jToolResponse:

  def save(
      result: Either[EngineError, Unit],
      successMessage: String
  ): String =
    result match
      case Left(error) =>
        s"Error: ${error.message}"
      case Right(_) =>
        s"Success: $successMessage"

  def render[A](
      result: Either[EngineError, A]
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
      s"speed: ${entity.speed.fold("none")(renderVector)}",
      s"weight: ${entity.weight.fold("none")(_.toString)}",
      s"health: ${entity.health.fold("none")(_.value.toString)}",
      s"team: ${entity.teamId.fold("none")(_.value)}"
    ).mkString("\n")

  def renderSurface(surface: Surface): String =
    List(
      s"id: ${surface.id.value}",
      s"position: ${renderVector(surface.position)}",
      s"shape: ${renderShape(surface.shape)}",
      s"frictionIndex: ${surface.frictionIndex.fold("none")(_.toString)}",
      s"appliedForce: ${surface.appliedForce.fold("none")(renderVector)}"
    ).mkString("\n")

  def renderTeam(team: Team): String =
    val enemies =
      if team.enemies.isEmpty then "none"
      else team.enemies.iterator.map(_.value).toList.sorted.mkString(", ")

    List(
      s"id: ${team.id.value}",
      s"enemies: $enemies"
    ).mkString("\n")

  def renderShape(shape: Shape2D): String =
    shape match
      case Shape2D.Circle(radius) =>
        s"circle, radius: $radius"
      case Shape2D.Rectangle(height, length) =>
        s"rectangle, height: $height, length: $length"

  def renderVector(vector: Vector2D): String =
    s"(${vector.x}, ${vector.y})"

  def withOptionalEntityFields(
      entity: Entity,
      teamId: String,
      weight: Integer,
      speedX: java.lang.Double,
      speedY: java.lang.Double
  ): Either[EngineError, Entity] =
    for
      entityWithTeam <- Option(teamId)
        .fold(Right(entity): Either[EngineError, Entity])(entity.withTeamId)
      entityWithWeight <- Option(weight)
        .fold(Right(entityWithTeam): Either[EngineError, Entity])(value =>
          entityWithTeam.withWeight(value.intValue())
        )
      completeEntity <- (
        (Option(speedX), Option(speedY)) match
          case (None, None) =>
            Right(entityWithWeight)
          case (Some(horizontal), Some(vertical)) =>
            Right(
              entityWithWeight.withSpeed(
                Vector2D(horizontal.doubleValue(), vertical.doubleValue())
              )
            )
          case _ =>
            Left(IncompleteEntitySpeed())
      ): Either[EngineError, Entity]
    yield completeEntity
