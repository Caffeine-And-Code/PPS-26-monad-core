package monad_core.simulator.infrastructure.ai

import dev.langchain4j.agent.tool.{P, Tool}
import monad_core.engine.model.*
import monad_core.engine.model.EntityBuilder.*
import monad_core.engine.model.SurfaceBuilder.*
import monad_core.simulator.application.engine.EngineControl
import monad_core.simulator.application.engine.errors.ErrorsAdapter.adaptError
import monad_core.simulator.application.engine.world.{
  SaveEntityCommand,
  SaveSurfaceCommand,
  SaveTeamCommand,
  World
}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.ai.Langchain4jToolResponse.*

final private[ai] case class IncompleteEntitySpeed()
    extends BaseError("Both speedX and speedY must be provided together")

final private[ai] case class IncompleteSurfaceAppliedForce()
    extends BaseError("Both appliedForceX and appliedForceY must be provided together")

case class Langchain4jTools()(using
    world: World,
    gameEngineRuntime: EngineControl
):

  private val defaultRotation = 0.0

  private val rotationOrDefault: java.lang.Double => Double =
    rotation => Option(rotation).fold(defaultRotation)(_.doubleValue())

  @Tool(Array("Lists all entities in the world."))
  def getAllEntities: String =
    renderList(world.getAllEntities, "entities")(renderEntity)

  @Tool(Array("Gets an entity by its identifier."))
  def getEntity(
      @P("Entity identifier") id: String
  ): String =
    render(LocatableId(id).adaptError().flatMap(id => world.getEntity(id.value)))(renderEntity)

  private def optionalVector(
      x: java.lang.Double,
      y: java.lang.Double,
      incompleteError: => BaseError
  ): Either[BaseError, Option[Vector2D]] =
    (Option(x), Option(y)) match
      case (None, None) => Right(None)
      case (Some(horizontal), Some(vertical)) =>
        Right(Some(Vector2D(horizontal.doubleValue(), vertical.doubleValue())))
      case _ => Left(incompleteError)

  private def configureEntity(
      entity: Either[EngineError, Entity],
      speed: Option[Vector2D],
      angularSpeed: Option[Double],
      weight: Option[Int],
      health: Option[Int],
      damage: Option[Int],
      teamId: Option[String]
  ): Either[BaseError, Entity] =
    entity
      .withSpeed(speed)
      .withAngularSpeed(angularSpeed)
      .withWeight(weight)
      .withHealth(health)
      .withDamage(damage)
      .withTeamId(teamId)
      .adaptError()

  private def configureSurface(
      surface: Either[EngineError, Surface],
      frictionIndex: Option[Double],
      appliedForce: Option[Vector2D],
      damageOverTime: Option[Int]
  ): Either[BaseError, Surface] =
    surface
      .withFrictionIndex(frictionIndex)
      .withAppliedForce(appliedForce)
      .withDamageOverTime(damageOverTime)
      .adaptError()

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
      speedY: java.lang.Double = null,
      @P(value = "Optional initial rotation in degrees, between 0 and 360", required = false)
      rotation: java.lang.Double = null,
      @P(value = "Optional angular speed in degrees per second", required = false)
      angularSpeed: java.lang.Double = null,
      @P(value = "Optional initial health of the entity", required = false)
      health: java.lang.Integer = null,
      @P(value = "Optional damage dealt by the entity", required = false)
      damage: java.lang.Integer = null
  ): String =
    whileEngineStopped {
      save(
        for
          speed <- optionalVector(speedX, speedY, IncompleteEntitySpeed())
          entity <- configureEntity(
            Entity.circle(id, Vector2D(x, y), radius, rotationOrDefault(rotation)),
            speed,
            Option(angularSpeed).map(_.toDouble),
            Option(weight).map(_.toInt),
            Option(health).map(_.toInt),
            Option(damage).map(_.toInt),
            Option(teamId)
          )
          scene <- world.createEntity(SaveEntityCommand(entity))
        yield scene,
        s"Entity '$id' created."
      )
    }

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
      speedY: java.lang.Double = null,
      @P(value = "Optional initial rotation in degrees, between 0 and 360", required = false)
      rotation: java.lang.Double = null,
      @P(value = "Optional angular speed in degrees per second", required = false)
      angularSpeed: java.lang.Double = null,
      @P(value = "Optional initial health of the entity", required = false)
      health: java.lang.Integer = null,
      @P(value = "Optional damage dealt by the entity", required = false)
      damage: java.lang.Integer = null
  ): String =
    whileEngineStopped {
      save(
        for
          speed <- optionalVector(speedX, speedY, IncompleteEntitySpeed())
          entity <- configureEntity(
            Entity.rectangle(id, Vector2D(x, y), height, length, rotationOrDefault(rotation)),
            speed,
            Option(angularSpeed).map(_.toDouble),
            Option(weight).map(_.toInt),
            Option(health).map(_.toInt),
            Option(damage).map(_.toInt),
            Option(teamId)
          )
          scene <- world.createEntity(SaveEntityCommand(entity))
        yield scene,
        s"Entity '$id' created."
      )
    }

  @Tool(Array("Replaces an entity with a circular entity having the same identifier."))
  def updateCircleEntity(
      @P("Identifier of the entity to update") id: String,
      @P("New X coordinate") x: Double,
      @P("New Y coordinate") y: Double,
      @P("New circle radius, greater than zero") radius: Double,
      @P(value = "Optional new rotation in degrees, between 0 and 360", required = false)
      rotation: java.lang.Double = null,
      @P(value = "Optional new angular speed in degrees per second", required = false)
      angularSpeed: java.lang.Double = null,
      @P(value = "Optional team identifier", required = false)
      teamId: String = null,
      @P(value = "Optional entity weight, zero or greater", required = false)
      weight: Integer = null,
      @P(value = "Optional horizontal speed; provide together with speedY", required = false)
      speedX: java.lang.Double = null,
      @P(value = "Optional vertical speed; provide together with speedX", required = false)
      speedY: java.lang.Double = null,
      @P(value = "Optional new health of the entity", required = false)
      health: java.lang.Integer = null,
      @P(value = "Optional new damage dealt by the entity", required = false)
      damage: java.lang.Integer = null
  ): String =
    whileEngineStopped {
      save(
        for
          speed <- optionalVector(speedX, speedY, IncompleteEntitySpeed())
          entity <- configureEntity(
            Entity.circle(id, Vector2D(x, y), radius, rotationOrDefault(rotation)),
            speed,
            Option(angularSpeed).map(_.toDouble),
            Option(weight).map(_.toInt),
            Option(health).map(_.toInt),
            Option(damage).map(_.toInt),
            Option(teamId)
          )
          scene <- world.updateEntity(SaveEntityCommand(entity))
        yield scene,
        s"Entity '$id' updated."
      )
    }

  @Tool(Array("Replaces an entity with a rectangular entity having the same identifier."))
  def updateRectangleEntity(
      @P("Identifier of the entity to update") id: String,
      @P("New X coordinate") x: Double,
      @P("New Y coordinate") y: Double,
      @P("New rectangle height, greater than zero") height: Double,
      @P("New rectangle length, greater than zero") length: Double,
      @P(value = "Optional new rotation in degrees, between 0 and 360", required = false)
      rotation: java.lang.Double = null,
      @P(value = "Optional new angular speed in degrees per second", required = false)
      angularSpeed: java.lang.Double = null,
      @P(value = "Optional team identifier", required = false)
      teamId: String = null,
      @P(value = "Optional entity weight, zero or greater", required = false)
      weight: Integer = null,
      @P(value = "Optional horizontal speed; provide together with speedY", required = false)
      speedX: java.lang.Double = null,
      @P(value = "Optional vertical speed; provide together with speedX", required = false)
      speedY: java.lang.Double = null,
      @P(value = "Optional new health of the entity", required = false)
      health: java.lang.Integer = null,
      @P(value = "Optional new damage dealt by the entity", required = false)
      damage: java.lang.Integer = null
  ): String =
    whileEngineStopped {
      save(
        result = for
          speed <- optionalVector(speedX, speedY, IncompleteEntitySpeed())
          entity <- configureEntity(
            Entity.rectangle(id, Vector2D(x, y), height, length, rotationOrDefault(rotation)),
            speed,
            Option(angularSpeed).map(_.toDouble),
            Option(weight).map(_.toInt),
            Option(health).map(_.toInt),
            Option(damage).map(_.toInt),
            Option(teamId)
          )
          scene <- world.updateEntity(SaveEntityCommand(entity))
        yield scene,
        successMessage = s"Entity '$id' updated."
      )
    }

  @Tool(Array("Removes an entity by its identifier."))
  def removeEntity(
      @P("Entity identifier") id: String
  ): String =
    whileEngineStopped {
      save(
        LocatableId(id).adaptError().flatMap(id => world.removeEntity(id.value)),
        s"Entity '$id' removed."
      )
    }

  @Tool(Array("Lists all surfaces in the world."))
  def getAllSurfaces: String =
    renderList(world.getAllSurfaces, "surfaces")(renderSurface)

  @Tool(Array("Gets a surface by its identifier."))
  def getSurface(
      @P("Surface identifier") id: String
  ): String =
    render(LocatableId(id).adaptError().flatMap(id => world.getSurface(id.value)))(renderSurface)

  @Tool(Array("Creates a circular surface."))
  def createCircleSurface(
      @P("Unique surface identifier") id: String,
      @P("X coordinate") x: Double,
      @P("Y coordinate") y: Double,
      @P("Circle radius, greater than zero") radius: Double,
      @P(value = "Optional initial rotation in degrees, between 0 and 360", required = false)
      rotation: java.lang.Double = null,
      @P(value = "Optional friction index", required = false)
      frictionIndex: java.lang.Double = null,
      @P(
        value = "Optional horizontal applied force; provide together with appliedForceY",
        required = false
      )
      appliedForceX: java.lang.Double = null,
      @P(
        value = "Optional vertical applied force; provide together with appliedForceX",
        required = false
      )
      appliedForceY: java.lang.Double = null,
      @P(value = "Optional damage over time dealt by the surface", required = false)
      damageOverTime: java.lang.Integer = null
  ): String =
    whileEngineStopped {
      save(
        for
          appliedForce <- optionalVector(
            appliedForceX,
            appliedForceY,
            IncompleteSurfaceAppliedForce()
          )
          surface <- configureSurface(
            Surface.circle(id, Vector2D(x, y), radius, rotationOrDefault(rotation)),
            Option(frictionIndex).map(_.toDouble),
            appliedForce,
            Option(damageOverTime).map(_.toInt)
          )
          scene <- world.createSurface(SaveSurfaceCommand(surface))
        yield scene,
        s"Surface '$id' created."
      )
    }

  @Tool(Array("Creates a rectangular surface."))
  def createRectangleSurface(
      @P("Unique surface identifier") id: String,
      @P("X coordinate") x: Double,
      @P("Y coordinate") y: Double,
      @P("Rectangle height, greater than zero") height: Double,
      @P("Rectangle length, greater than zero") length: Double,
      @P(value = "Optional initial rotation in degrees, between 0 and 360", required = false)
      rotation: java.lang.Double = null,
      @P(value = "Optional friction index", required = false)
      frictionIndex: java.lang.Double = null,
      @P(
        value = "Optional horizontal applied force; provide together with appliedForceY",
        required = false
      )
      appliedForceX: java.lang.Double = null,
      @P(
        value = "Optional vertical applied force; provide together with appliedForceX",
        required = false
      )
      appliedForceY: java.lang.Double = null,
      @P(value = "Optional damage over time dealt by the surface", required = false)
      damageOverTime: java.lang.Integer = null
  ): String =
    whileEngineStopped {
      save(
        for
          appliedForce <- optionalVector(
            appliedForceX,
            appliedForceY,
            IncompleteSurfaceAppliedForce()
          )
          surface <- configureSurface(
            Surface.rectangle(id, Vector2D(x, y), height, length, rotationOrDefault(rotation)),
            Option(frictionIndex).map(_.toDouble),
            appliedForce,
            Option(damageOverTime).map(_.toInt)
          )
          scene <- world.createSurface(SaveSurfaceCommand(surface))
        yield scene,
        s"Surface '$id' created."
      )
    }

  @Tool(Array("Replaces a surface with a circular surface having the same identifier."))
  def updateCircleSurface(
      @P("Identifier of the surface to update") id: String,
      @P("New X coordinate") x: Double,
      @P("New Y coordinate") y: Double,
      @P("New circle radius, greater than zero") radius: Double,
      @P(value = "Optional new rotation in degrees, between 0 and 360", required = false)
      rotation: java.lang.Double = null,
      @P(value = "Optional new friction index", required = false)
      frictionIndex: java.lang.Double = null,
      @P(
        value = "Optional new horizontal applied force; provide together with appliedForceY",
        required = false
      )
      appliedForceX: java.lang.Double = null,
      @P(
        value = "Optional new vertical applied force; provide together with appliedForceX",
        required = false
      )
      appliedForceY: java.lang.Double = null,
      @P(value = "Optional new damage over time dealt by the surface", required = false)
      damageOverTime: java.lang.Integer = null
  ): String =
    whileEngineStopped {
      save(
        for
          appliedForce <- optionalVector(
            appliedForceX,
            appliedForceY,
            IncompleteSurfaceAppliedForce()
          )
          surface <- configureSurface(
            Surface.circle(id, Vector2D(x, y), radius, rotationOrDefault(rotation)),
            Option(frictionIndex).map(_.toDouble),
            appliedForce,
            Option(damageOverTime).map(_.toInt)
          )
          scene <- world.updateSurface(SaveSurfaceCommand(surface))
        yield scene,
        s"Surface '$id' updated."
      )
    }

  @Tool(Array("Replaces a surface with a rectangular surface having the same identifier."))
  def updateRectangleSurface(
      @P("Identifier of the surface to update") id: String,
      @P("New X coordinate") x: Double,
      @P("New Y coordinate") y: Double,
      @P("New rectangle height, greater than zero") height: Double,
      @P("New rectangle length, greater than zero") length: Double,
      @P(value = "Optional new rotation in degrees, between 0 and 360", required = false)
      rotation: java.lang.Double = null,
      @P(value = "Optional new friction index", required = false)
      frictionIndex: java.lang.Double = null,
      @P(
        value = "Optional new horizontal applied force; provide together with appliedForceY",
        required = false
      )
      appliedForceX: java.lang.Double = null,
      @P(
        value = "Optional new vertical applied force; provide together with appliedForceX",
        required = false
      )
      appliedForceY: java.lang.Double = null,
      @P(value = "Optional new damage over time dealt by the surface", required = false)
      damageOverTime: java.lang.Integer = null
  ): String =
    whileEngineStopped {
      save(
        for
          appliedForce <- optionalVector(
            appliedForceX,
            appliedForceY,
            IncompleteSurfaceAppliedForce()
          )
          surface <- configureSurface(
            Surface.rectangle(id, Vector2D(x, y), height, length, rotationOrDefault(rotation)),
            Option(frictionIndex).map(_.toDouble),
            appliedForce,
            Option(damageOverTime).map(_.toInt)
          )
          scene <- world.updateSurface(SaveSurfaceCommand(surface))
        yield scene,
        s"Surface '$id' updated."
      )
    }

  @Tool(Array("Removes a surface by its identifier."))
  def removeSurface(
      @P("Surface identifier") id: String
  ): String =
    whileEngineStopped {
      save(
        LocatableId(id).adaptError().flatMap(id => world.removeSurface(id.value)),
        s"Surface '$id' removed."
      )
    }

  @Tool(Array("Lists all teams in the world."))
  def getAllTeams: String =
    renderList(world.getAllTeams, "teams")(renderTeam)

  @Tool(Array("Gets a team by its identifier."))
  def getTeam(
      @P("Team identifier") id: String
  ): String =
    render(TeamId(id).adaptError().flatMap(id => world.getTeam(id.value)))(renderTeam)

  @Tool(Array("Creates a team and optionally assigns enemy teams."))
  def createTeam(
      @P("Unique team identifier") id: String,
      @P("Comma-separated enemy team identifiers; use an empty string for none") enemies: String
  ): String =
    whileEngineStopped {
      save(
        Team
          .create(id, parseIds(enemies))
          .adaptError()
          .flatMap(team => world.createTeam(SaveTeamCommand(team))),
        s"Team '$id' created."
      )
    }

  @Tool(Array("Replaces a team's enemy list."))
  def updateTeam(
      @P("Identifier of the team to update") id: String,
      @P("New comma-separated enemy team identifiers; use an empty string for none") enemies: String
  ): String =
    whileEngineStopped {
      save(
        Team
          .create(id, parseIds(enemies))
          .adaptError()
          .flatMap(team => world.updateTeam(SaveTeamCommand(team))),
        s"Team '$id' updated."
      )
    }

  @Tool(Array("Removes a team by its identifier."))
  def removeTeam(
      @P("Team identifier") id: String
  ): String =
    whileEngineStopped {
      save(
        TeamId(id).adaptError().flatMap(id => world.removeTeam(id.value)),
        s"Team '$id' removed."
      )
    }

  @Tool(Array("Starts the game engine."))
  def start(): String =
    gameEngineRuntime.start()
    "Game engine started."

  @Tool(Array("Stops the game engine."))
  def stop(): String =
    gameEngineRuntime.stop()
    "Game engine stopped."

  private def whileEngineStopped(operation: => String): String =
    if gameEngineRuntime.isRunning then
      "Error: The world cannot be modified while the game engine is running."
    else operation

  private def parseIds(csv: String): Set[String] =
    csv.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSet
