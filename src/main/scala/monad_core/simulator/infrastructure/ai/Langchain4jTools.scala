package monad_core.simulator.infrastructure.ai

import dev.langchain4j.agent.tool.{P, Tool}
import monad_core.engine.core.Scene
import monad_core.engine.core.traits.State
import monad_core.engine.errors.EngineError
import monad_core.engine.model.*
import monad_core.simulator.application.engine.*
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.engine.EngineWord

case class IncompleteEntitySpeed() extends EngineError("Both speedX and speedY must be provided together")

case class Langchain4jTools()(
  using initialWord: Word,
  gameEngineRuntime: GameEngineRuntime
):
  private var word: Word = initialWord

  @Tool(Array("Lists all entities in the world."))
  def getAllEntities: String =
    renderList(word.getAllEntities, "entities")(renderEntity)

  @Tool(Array("Gets an entity by its identifier."))
  def getEntity(
    @P("Entity identifier") id: String
  ): String =
    render(LocatableId(id).flatMap(word.getEntity))(renderEntity)

  @Tool(Array("Creates a circular entity."))
  def createCircleEntity(
    @P("Unique entity identifier") id: String,
    @P("X coordinate") x: Double,
    @P("Y coordinate") y: Double,
    @P("Circle radius, greater than zero") radius: Double,
    @P(value = "Optional team identifier", required = false)
    teamId: String = null,
    @P(value = "Optional entity weight, zero or greater", required = false)
    weight: Integer = null,
    @P(value = "Optional horizontal speed; provide together with speedY", required = false)
    speedX: java.lang.Double = null,
    @P(value = "Optional vertical speed; provide together with speedX", required = false)
    speedY: java.lang.Double = null
  ): String =
    save(
      Entity.circle(id, Vector2D(x, y), radius)
        .flatMap(withOptionalEntityFields(_, teamId, weight, speedX, speedY))
        .flatMap(entity => word.createEntity(SaveEntityCommand(entity))),
      s"Entity '$id' created."
    )

  @Tool(Array("Creates a rectangular entity."))
  def createRectangleEntity(
    @P("Unique entity identifier") id: String,
    @P("X coordinate") x: Double,
    @P("Y coordinate") y: Double,
    @P("Rectangle height, greater than zero") height: Double,
    @P("Rectangle length, greater than zero") length: Double,
    @P(value = "Optional team identifier", required = false)
    teamId: String = null,
    @P(value = "Optional entity weight, zero or greater", required = false)
    weight: Integer = null,
    @P(value = "Optional horizontal speed; provide together with speedY", required = false)
    speedX: java.lang.Double = null,
    @P(value = "Optional vertical speed; provide together with speedX", required = false)
    speedY: java.lang.Double = null
  ): String =
    save(
      Entity.rectangle(id, Vector2D(x, y), height, length)
        .flatMap(withOptionalEntityFields(_, teamId, weight, speedX, speedY))
        .flatMap(entity => word.createEntity(SaveEntityCommand(entity))),
      s"Entity '$id' created."
    )

  @Tool(Array("Replaces an entity with a circular entity having the same identifier."))
  def updateCircleEntity(
    @P("Identifier of the entity to update") id: String,
    @P("New X coordinate") x: Double,
    @P("New Y coordinate") y: Double,
    @P("New circle radius, greater than zero") radius: Double
  ): String =
    save(
      Entity.circle(id, Vector2D(x, y), radius)
        .flatMap(entity => word.updateEntity(SaveEntityCommand(entity))),
      s"Entity '$id' updated."
    )

  @Tool(Array("Replaces an entity with a rectangular entity having the same identifier."))
  def updateRectangleEntity(
    @P("Identifier of the entity to update") id: String,
    @P("New X coordinate") x: Double,
    @P("New Y coordinate") y: Double,
    @P("New rectangle height, greater than zero") height: Double,
    @P("New rectangle length, greater than zero") length: Double
  ): String =
    save(
      Entity.rectangle(id, Vector2D(x, y), height, length)
        .flatMap(entity => word.updateEntity(SaveEntityCommand(entity))),
      s"Entity '$id' updated."
    )

  @Tool(Array("Removes an entity by its identifier."))
  def removeEntity(
    @P("Entity identifier") id: String
  ): String =
    save(
      LocatableId(id).flatMap(word.removeEntity),
      s"Entity '$id' removed."
    )

  @Tool(Array("Lists all surfaces in the world."))
  def getAllSurfaces: String =
    renderList(word.getAllSurfaces, "surfaces")(renderSurface)

  @Tool(Array("Gets a surface by its identifier."))
  def getSurface(
    @P("Surface identifier") id: String
  ): String =
    render(LocatableId(id).flatMap(word.getSurface))(renderSurface)

  @Tool(Array("Creates a circular surface."))
  def createCircleSurface(
    @P("Unique surface identifier") id: String,
    @P("X coordinate") x: Double,
    @P("Y coordinate") y: Double,
    @P("Circle radius, greater than zero") radius: Double
  ): String =
    save(
      Surface.circle(id, Vector2D(x, y), radius)
        .flatMap(surface => word.createSurface(SaveSurfaceCommand(surface))),
      s"Surface '$id' created."
    )

  @Tool(Array("Creates a rectangular surface."))
  def createRectangleSurface(
    @P("Unique surface identifier") id: String,
    @P("X coordinate") x: Double,
    @P("Y coordinate") y: Double,
    @P("Rectangle height, greater than zero") height: Double,
    @P("Rectangle length, greater than zero") length: Double
  ): String =
    save(
      Surface.rectangle(id, Vector2D(x, y), height, length)
        .flatMap(surface => word.createSurface(SaveSurfaceCommand(surface))),
      s"Surface '$id' created."
    )

  @Tool(Array("Replaces a surface with a circular surface having the same identifier."))
  def updateCircleSurface(
    @P("Identifier of the surface to update") id: String,
    @P("New X coordinate") x: Double,
    @P("New Y coordinate") y: Double,
    @P("New circle radius, greater than zero") radius: Double
  ): String =
    save(
      Surface.circle(id, Vector2D(x, y), radius)
        .flatMap(surface => word.updateSurface(SaveSurfaceCommand(surface))),
      s"Surface '$id' updated."
    )

  @Tool(Array("Replaces a surface with a rectangular surface having the same identifier."))
  def updateRectangleSurface(
    @P("Identifier of the surface to update") id: String,
    @P("New X coordinate") x: Double,
    @P("New Y coordinate") y: Double,
    @P("New rectangle height, greater than zero") height: Double,
    @P("New rectangle length, greater than zero") length: Double
  ): String =
    save(
      Surface.rectangle(id, Vector2D(x, y), height, length)
        .flatMap(surface => word.updateSurface(SaveSurfaceCommand(surface))),
      s"Surface '$id' updated."
    )

  @Tool(Array("Removes a surface by its identifier."))
  def removeSurface(
    @P("Surface identifier") id: String
  ): String =
    save(
      LocatableId(id).flatMap(word.removeSurface),
      s"Surface '$id' removed."
    )

  @Tool(Array("Lists all teams in the world."))
  def getAllTeams: String =
    renderList(word.getAllTeams, "teams")(renderTeam)

  @Tool(Array("Gets a team by its identifier."))
  def getTeam(
    @P("Team identifier") id: String
  ): String =
    render(TeamId(id).flatMap(word.getTeam))(renderTeam)

  @Tool(Array("Creates a team and optionally assigns enemy teams."))
  def createTeam(
    @P("Unique team identifier") id: String,
    @P("Comma-separated enemy team identifiers; use an empty string for none") enemies: String
  ): String =
    save(
      Team.create(id, parseIds(enemies))
        .flatMap(team => word.createTeam(SaveTeamCommand(team))),
      s"Team '$id' created."
    )

  @Tool(Array("Replaces a team's enemy list."))
  def updateTeam(
    @P("Identifier of the team to update") id: String,
    @P("New comma-separated enemy team identifiers; use an empty string for none") enemies: String
  ): String =
    save(
      Team.create(id, parseIds(enemies))
        .flatMap(team => word.updateTeam(SaveTeamCommand(team))),
      s"Team '$id' updated."
    )

  @Tool(Array("Removes a team by its identifier."))
  def removeTeam(
    @P("Team identifier") id: String
  ): String =
    save(
      TeamId(id).flatMap(word.removeTeam),
      s"Team '$id' removed."
    )

  @Tool(Array("Starts the game engine."))
  def start(): String =
    gameEngineRuntime.start()
    "Game engine started."

  @Tool(Array("Stops the game engine."))
  def stop(): String =
    gameEngineRuntime.stop()
    "Game engine stopped."

  private def save(
    result: Either[EngineError, State],
    successMessage: String
  ): String =
    result match
      case Left(error) =>
        s"Error: ${error.message}"
      case Right(scene: Scene) =>
        word = EngineWord(scene)
        s"Success: $successMessage"
      case Right(_) =>
        "Error: the operation returned an unsupported world state."

  private def render[A](
    result: Either[EngineError, A]
  )(
    format: A => String
  ): String =
    result.fold(
      error => s"Error: ${error.message}",
      value => s"Result:\n${format(value)}"
    )

  private def renderList[A](
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

  private def renderEntity(entity: Entity): String =
    List(
      s"id: ${entity.id.value}",
      s"position: ${renderVector(entity.position)}",
      s"shape: ${renderShape(entity.shape)}",
      s"speed: ${entity.speed.fold("none")(renderVector)}",
      s"weight: ${entity.weight.fold("none")(_.toString)}",
      s"health: ${entity.health.fold("none")(_.value.toString)}",
      s"team: ${entity.teamId.fold("none")(_.value)}"
    ).mkString("\n")

  private def renderSurface(surface: Surface): String =
    List(
      s"id: ${surface.id.value}",
      s"position: ${renderVector(surface.position)}",
      s"shape: ${renderShape(surface.shape)}",
      s"frictionIndex: ${surface.frictionIndex.fold("none")(_.toString)}",
      s"appliedForce: ${surface.appliedForce.fold("none")(renderVector)}"
    ).mkString("\n")

  private def renderTeam(team: Team): String =
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

  private def parseIds(csv: String): Set[String] =
    csv.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSet

  private def withOptionalEntityFields(
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
        .fold(Right(entityWithTeam): Either[EngineError, Entity])(
          value => entityWithTeam.withWeight(value.intValue())
        )
      completeEntity <- (
        (Option(speedX), Option(speedY)) match
          case (None, None) =>
            Right(entityWithWeight)
          case (Some(horizontal), Some(vertical)) =>
            entityWithWeight.withSpeed(
              Vector2D(horizontal.doubleValue(), vertical.doubleValue())
            )
          case _ =>
            Left(IncompleteEntitySpeed())
      ): Either[EngineError, Entity]
    yield completeEntity
