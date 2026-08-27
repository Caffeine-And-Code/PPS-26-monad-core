package monad_core.simulator.infrastructure.ai

import monad_core.engine.model.*
import monad_core.engine.simulator.Painter
import monad_core.simulator.application.engine.*
import monad_core.simulator.application.engine.world.{
  SaveEntityCommand,
  SaveSurfaceCommand,
  SaveTeamCommand,
  World
}
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.compiletime.uninitialized

class Langchain4jToolsTest
    extends AnyFunSuite
    with Matchers
    with MockFactory
    with BeforeAndAfterEach:

  private val entityId        = "entity"
  private val surfaceId       = "surface"
  private val posX            = 2.0
  private val posY            = 6.0
  private val radius          = 3.0
  private val height          = 4.0
  private val rectangleLength = 5.0
  private val defaultRotation = 0.0
  private val rotation        = 45.0
  private val angularSpeed    = -30.0
  private val frictionIndex   = 0.25
  private val appliedForce    = Vector2D(10.0, -4.0)
  private val damageOverTime  = 3
  private val invalidId       = ""

  private var world: World                         = uninitialized
  private var gameEngineRuntime: GameEngineRuntime = uninitialized

  override def beforeEach(): Unit =
    super.beforeEach()
    world = mock[World]
    gameEngineRuntime = mock[GameEngineRuntime]

  private def tools: Langchain4jTools =
    Langchain4jTools()(using world, gameEngineRuntime)

  private def withAllOptionalFields(
      surface: Either[EngineError, Surface]
  ): Surface =
    surface
      .flatMap(_.withFrictionIndex(Some(frictionIndex)))
      .flatMap(_.withAppliedForce(Some(appliedForce)))
      .flatMap(_.withDamageOverTime(Some(damageOverTime)))
      .value

  test("list all entities in the world returns empty if scene is empty"):
    (() => world.getAllEntities).expects().returning(List.empty).once()

    val result = tools.getAllEntities

    result shouldBe "Result: no entities found."

  test("list all entities in the world returns the list"):
    val circle = Entity.circle("circle", Vector2D(posX, posY), radius).value
    val rectangle =
      Entity.rectangle("rectangle", Vector2D(posX, posY), height, rectangleLength).value
    (() => world.getAllEntities).expects().returning(List(circle, rectangle)).once()

    val result = tools.getAllEntities

    result shouldBe
      s"""Result: 2 entities found.
         |1:
         |id: circle
         |position: ($posX, $posY)
         |shape: circle, radius: $radius
         |rotation: $defaultRotation
         |speed: none
         |angularSpeed: none
         |weight: none
         |health: none
         |damage: none
         |team: none
         |
         |2:
         |id: rectangle
         |position: ($posX, $posY)
         |shape: rectangle, height: $height, length: $rectangleLength
         |rotation: $defaultRotation
         |speed: none
         |angularSpeed: none
         |weight: none
         |health: none
         |damage: none
         |team: none""".stripMargin

  test("when get entity is called returns the formatted entity"):
    val entity = Entity.circle(entityId, Vector2D(posX, posY), radius).value
    world.getEntity.expects(LocatableId(entityId).value.value).returning(Right(entity)).once()

    val result = tools.getEntity(entityId)

    result shouldBe
      s"""Result:
         |id: $entityId
         |position: ($posX, $posY)
         |shape: circle, radius: $radius
         |rotation: $defaultRotation
         |speed: none
         |angularSpeed: none
         |weight: none
         |health: none
         |damage: none
         |team: none""".stripMargin

  test("when get entity receives an invalid id returns an error"):
    val result = tools.getEntity(invalidId)

    result shouldBe "Error: LocatableId cannot be empty"

  test("when create circle entity is called a success message is returned"):
    val entity = Entity.circle(entityId, Vector2D(posX, posY), radius).value
    world.createEntity
      .expects(SaveEntityCommand(entity))
      .returning(Right(Scene(entities = Map(entity.id -> entity))))
      .once()

    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.createCircleEntity(entityId, posX, posY, radius)

    result shouldBe s"Success: Entity '$entityId' created."

  test("when create circle entity receives optional fields they are saved"):
    val teamId = "blue"
    val weight = 12
    val speedX = 1.5
    val speedY = -2.5
    val health = 100
    val damage = 7
    val entity = Entity
      .circle(entityId, Vector2D(posX, posY), radius)
      .flatMap(_.withTeamId(Some(teamId)))
      .flatMap(_.withWeight(Some(weight)))
      .map(_.withSpeed(Some(Vector2D(speedX, speedY))))
      .flatMap(_.withHealth(Some(health)))
      .flatMap(_.withDamage(Some(damage)))
      .value
    world.createEntity
      .expects(SaveEntityCommand(entity))
      .returning(Right(Scene(entities = Map(entity.id -> entity))))
      .once()

    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.createCircleEntity(
      entityId,
      posX,
      posY,
      radius,
      teamId,
      Integer.valueOf(weight),
      java.lang.Double.valueOf(speedX),
      java.lang.Double.valueOf(speedY),
      health = Integer.valueOf(health),
      damage = Integer.valueOf(damage)
    )

    result shouldBe s"Success: Entity '$entityId' created."

  test("when create entity receives only one speed component it returns an error"):
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.createCircleEntity(
      entityId,
      posX,
      posY,
      radius,
      speedX = java.lang.Double.valueOf(1.5)
    )

    result shouldBe "Error: Both speedX and speedY must be provided together"

  test("when create circle entity receives an invalid radius returns an error"):
    val invalidRadius = -radius

    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.createCircleEntity(entityId, posX, posY, invalidRadius)

    result shouldBe "Error: Radius must be greater than 0"

  test("when create rectangle entity is called a success message is returned"):
    val entity = Entity.rectangle(entityId, Vector2D(posX, posY), height, rectangleLength).value
    world.createEntity
      .expects(SaveEntityCommand(entity))
      .returning(Right(Scene(entities = Map(entity.id -> entity))))
      .once()

    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.createRectangleEntity(entityId, posX, posY, height, rectangleLength)

    result shouldBe s"Success: Entity '$entityId' created."

  test("when create rectangle entity receives a rotation it is saved"):
    val entity =
      Entity.rectangle(entityId, Vector2D(posX, posY), height, rectangleLength, rotation).value
    world.createEntity
      .expects(SaveEntityCommand(entity))
      .returning(Right(Scene(entities = Map(entity.id -> entity))))
      .once()
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.createRectangleEntity(
      entityId,
      posX,
      posY,
      height,
      rectangleLength,
      rotation = java.lang.Double.valueOf(rotation)
    )

    result shouldBe s"Success: Entity '$entityId' created."

  test("when create circle entity receives a rotation it is saved"):
    val entity = Entity.circle(entityId, Vector2D(posX, posY), radius, rotation).value
    world.createEntity
      .expects(SaveEntityCommand(entity))
      .returning(Right(Scene(entities = Map(entity.id -> entity))))
      .once()
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.createCircleEntity(
      entityId,
      posX,
      posY,
      radius,
      rotation = java.lang.Double.valueOf(rotation)
    )

    result shouldBe s"Success: Entity '$entityId' created."

  test("when create circle entity receives an angular speed it is saved"):
    val entity = Entity
      .circle(entityId, Vector2D(posX, posY), radius)
      .map(_.withAngularSpeed(Some(angularSpeed)))
      .value
    world.createEntity
      .expects(SaveEntityCommand(entity))
      .returning(Right(Scene(entities = Map(entity.id -> entity))))
      .once()
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.createCircleEntity(
      entityId,
      posX,
      posY,
      radius,
      angularSpeed = java.lang.Double.valueOf(angularSpeed)
    )

    result shouldBe s"Success: Entity '$entityId' created."

  test("when create rectangle entity receives an angular speed it is saved"):
    val entity = Entity
      .rectangle(entityId, Vector2D(posX, posY), height, rectangleLength)
      .map(_.withAngularSpeed(Some(angularSpeed)))
      .value
    world.createEntity
      .expects(SaveEntityCommand(entity))
      .returning(Right(Scene(entities = Map(entity.id -> entity))))
      .once()
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.createRectangleEntity(
      entityId,
      posX,
      posY,
      height,
      rectangleLength,
      angularSpeed = java.lang.Double.valueOf(angularSpeed)
    )

    result shouldBe s"Success: Entity '$entityId' created."

  test("when update circle entity is called returns a success message"):
    val entity = Entity.circle(entityId, Vector2D(posX, posY), radius).value
    world.updateEntity
      .expects(SaveEntityCommand(entity))
      .returning(Right(Scene(entities = Map(entity.id -> entity))))
      .once()

    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.updateCircleEntity(entityId, posX, posY, radius)

    result shouldBe s"Success: Entity '$entityId' updated."

  test("when update rectangle entity is called returns a success message"):
    val entity = Entity.rectangle(entityId, Vector2D(posX, posY), height, rectangleLength).value
    world.updateEntity
      .expects(SaveEntityCommand(entity))
      .returning(Right(Scene(entities = Map(entity.id -> entity))))
      .once()

    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.updateRectangleEntity(entityId, posX, posY, height, rectangleLength)

    result shouldBe s"Success: Entity '$entityId' updated."

  test("when update rectangle entity receives a rotation it is saved"):
    val entity =
      Entity.rectangle(entityId, Vector2D(posX, posY), height, rectangleLength, rotation).value
    world.updateEntity
      .expects(SaveEntityCommand(entity))
      .returning(Right(Scene(entities = Map(entity.id -> entity))))
      .once()
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.updateRectangleEntity(
      entityId,
      posX,
      posY,
      height,
      rectangleLength,
      rotation = java.lang.Double.valueOf(rotation)
    )

    result shouldBe s"Success: Entity '$entityId' updated."

  test("when update circle entity receives a rotation it is saved"):
    val entity = Entity.circle(entityId, Vector2D(posX, posY), radius, rotation).value
    world.updateEntity
      .expects(SaveEntityCommand(entity))
      .returning(Right(Scene(entities = Map(entity.id -> entity))))
      .once()
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.updateCircleEntity(
      entityId,
      posX,
      posY,
      radius,
      rotation = java.lang.Double.valueOf(rotation)
    )

    result shouldBe s"Success: Entity '$entityId' updated."

  test("when update circle entity receives an angular speed it is saved"):
    val entity = Entity
      .circle(entityId, Vector2D(posX, posY), radius)
      .map(_.withAngularSpeed(Some(angularSpeed)))
      .value
    world.updateEntity
      .expects(SaveEntityCommand(entity))
      .returning(Right(Scene(entities = Map(entity.id -> entity))))
      .once()
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.updateCircleEntity(
      entityId,
      posX,
      posY,
      radius,
      angularSpeed = java.lang.Double.valueOf(angularSpeed)
    )

    result shouldBe s"Success: Entity '$entityId' updated."

  test("when update rectangle entity receives an angular speed it is saved"):
    val entity = Entity
      .rectangle(entityId, Vector2D(posX, posY), height, rectangleLength)
      .map(_.withAngularSpeed(Some(angularSpeed)))
      .value
    world.updateEntity
      .expects(SaveEntityCommand(entity))
      .returning(Right(Scene(entities = Map(entity.id -> entity))))
      .once()
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.updateRectangleEntity(
      entityId,
      posX,
      posY,
      height,
      rectangleLength,
      angularSpeed = java.lang.Double.valueOf(angularSpeed)
    )

    result shouldBe s"Success: Entity '$entityId' updated."

  test("when remove entity is called returns a success message"):
    val id = LocatableId(entityId).value
    world.removeEntity.expects(id.value).returning(Right(Scene())).once()

    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.removeEntity(entityId)

    result shouldBe s"Success: Entity '$entityId' removed."

  test("list all surfaces in the world returns empty if scene is empty"):
    (() => world.getAllSurfaces).expects().returning(List.empty).once()

    val result = tools.getAllSurfaces

    result shouldBe "Result: no surfaces found."

  test("list all surfaces in the world returns the list"):
    val surface = Surface.circle(surfaceId, Vector2D(posX, posY), radius).value
    (() => world.getAllSurfaces).expects().returning(List(surface)).once()

    val result = tools.getAllSurfaces

    result shouldBe
      s"""Result: 1 surfaces found.
         |1:
         |id: $surfaceId
         |position: ($posX, $posY)
         |shape: circle, radius: $radius
         |rotation: $defaultRotation
         |frictionIndex: none
         |appliedForce: none
         |damageOverTime: none""".stripMargin

  test("when get surface is called returns the formatted surface"):
    val surface = Surface.rectangle(surfaceId, Vector2D(posX, posY), height, rectangleLength).value
    world.getSurface.expects(LocatableId(surfaceId).value.value).returning(Right(surface)).once()

    val result = tools.getSurface(surfaceId)

    result shouldBe
      s"""Result:
         |id: $surfaceId
         |position: ($posX, $posY)
         |shape: rectangle, height: $height, length: $rectangleLength
         |rotation: $defaultRotation
         |frictionIndex: none
         |appliedForce: none
         |damageOverTime: none""".stripMargin

  test("when create circle surface is called returns a success message"):
    val surface = Surface.circle(surfaceId, Vector2D(posX, posY), radius).value
    world.createSurface
      .expects(SaveSurfaceCommand(surface))
      .returning(Right(Scene(surfaces = Map(surface.id -> surface))))
      .once()

    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.createCircleSurface(surfaceId, posX, posY, radius)

    result shouldBe s"Success: Surface '$surfaceId' created."

  test("when create rectangle surface is called returns a success message"):
    val surface = Surface.rectangle(surfaceId, Vector2D(posX, posY), height, rectangleLength).value
    world.createSurface
      .expects(SaveSurfaceCommand(surface))
      .returning(Right(Scene(surfaces = Map(surface.id -> surface))))
      .once()

    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.createRectangleSurface(surfaceId, posX, posY, height, rectangleLength)

    result shouldBe s"Success: Surface '$surfaceId' created."

  test("when create circle surface receives optional fields they are saved"):
    val surface = withAllOptionalFields(
      Surface.circle(surfaceId, Vector2D(posX, posY), radius)
    )
    world.createSurface
      .expects(SaveSurfaceCommand(surface))
      .returning(Right(Scene(surfaces = Map(surface.id -> surface))))
      .once()
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.createCircleSurface(
      surfaceId,
      posX,
      posY,
      radius,
      frictionIndex = java.lang.Double.valueOf(frictionIndex),
      appliedForceX = java.lang.Double.valueOf(appliedForce.x),
      appliedForceY = java.lang.Double.valueOf(appliedForce.y),
      damageOverTime = Integer.valueOf(damageOverTime)
    )

    result shouldBe s"Success: Surface '$surfaceId' created."

  test("when create rectangle surface receives optional fields they are saved"):
    val surface = withAllOptionalFields(
      Surface.rectangle(surfaceId, Vector2D(posX, posY), height, rectangleLength)
    )
    world.createSurface
      .expects(SaveSurfaceCommand(surface))
      .returning(Right(Scene(surfaces = Map(surface.id -> surface))))
      .once()
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.createRectangleSurface(
      surfaceId,
      posX,
      posY,
      height,
      rectangleLength,
      frictionIndex = java.lang.Double.valueOf(frictionIndex),
      appliedForceX = java.lang.Double.valueOf(appliedForce.x),
      appliedForceY = java.lang.Double.valueOf(appliedForce.y),
      damageOverTime = Integer.valueOf(damageOverTime)
    )

    result shouldBe s"Success: Surface '$surfaceId' created."

  test("when update circle surface is called returns a success message"):
    val surface = Surface.circle(surfaceId, Vector2D(posX, posY), radius).value
    world.updateSurface
      .expects(SaveSurfaceCommand(surface))
      .returning(Right(Scene(surfaces = Map(surface.id -> surface))))
      .once()

    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.updateCircleSurface(surfaceId, posX, posY, radius)

    result shouldBe s"Success: Surface '$surfaceId' updated."

  test("when update rectangle surface is called returns a success message"):
    val surface = Surface.rectangle(surfaceId, Vector2D(posX, posY), height, rectangleLength).value
    world.updateSurface
      .expects(SaveSurfaceCommand(surface))
      .returning(Right(Scene(surfaces = Map(surface.id -> surface))))
      .once()

    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.updateRectangleSurface(surfaceId, posX, posY, height, rectangleLength)

    result shouldBe s"Success: Surface '$surfaceId' updated."

  test("when update circle surface receives optional fields they are saved"):
    val surface = withAllOptionalFields(
      Surface.circle(surfaceId, Vector2D(posX, posY), radius)
    )
    world.updateSurface
      .expects(SaveSurfaceCommand(surface))
      .returning(Right(Scene(surfaces = Map(surface.id -> surface))))
      .once()
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.updateCircleSurface(
      surfaceId,
      posX,
      posY,
      radius,
      frictionIndex = java.lang.Double.valueOf(frictionIndex),
      appliedForceX = java.lang.Double.valueOf(appliedForce.x),
      appliedForceY = java.lang.Double.valueOf(appliedForce.y),
      damageOverTime = Integer.valueOf(damageOverTime)
    )

    result shouldBe s"Success: Surface '$surfaceId' updated."

  test("when update rectangle surface receives optional fields they are saved"):
    val surface = withAllOptionalFields(
      Surface.rectangle(surfaceId, Vector2D(posX, posY), height, rectangleLength)
    )
    world.updateSurface
      .expects(SaveSurfaceCommand(surface))
      .returning(Right(Scene(surfaces = Map(surface.id -> surface))))
      .once()
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.updateRectangleSurface(
      surfaceId,
      posX,
      posY,
      height,
      rectangleLength,
      frictionIndex = java.lang.Double.valueOf(frictionIndex),
      appliedForceX = java.lang.Double.valueOf(appliedForce.x),
      appliedForceY = java.lang.Double.valueOf(appliedForce.y),
      damageOverTime = Integer.valueOf(damageOverTime)
    )

    result shouldBe s"Success: Surface '$surfaceId' updated."

  test("when a surface receives only one applied force component it returns an error"):
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.createCircleSurface(
      surfaceId,
      posX,
      posY,
      radius,
      appliedForceX = java.lang.Double.valueOf(appliedForce.x)
    )

    result shouldBe "Error: Both appliedForceX and appliedForceY must be provided together"

  test("when update rectangle surface receives a rotation it is saved"):
    val surface =
      Surface.rectangle(surfaceId, Vector2D(posX, posY), height, rectangleLength, rotation).value
    world.updateSurface
      .expects(SaveSurfaceCommand(surface))
      .returning(Right(Scene(surfaces = Map(surface.id -> surface))))
      .once()
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.updateRectangleSurface(
      surfaceId,
      posX,
      posY,
      height,
      rectangleLength,
      rotation = java.lang.Double.valueOf(rotation)
    )

    result shouldBe s"Success: Surface '$surfaceId' updated."

  test("when update circle surface receives a rotation it is saved"):
    val surface = Surface.circle(surfaceId, Vector2D(posX, posY), radius, rotation).value
    world.updateSurface
      .expects(SaveSurfaceCommand(surface))
      .returning(Right(Scene(surfaces = Map(surface.id -> surface))))
      .once()
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.updateCircleSurface(
      surfaceId,
      posX,
      posY,
      radius,
      rotation = java.lang.Double.valueOf(rotation)
    )

    result shouldBe s"Success: Surface '$surfaceId' updated."

  test("when create circle surface receives a rotation it is saved"):
    val surface = Surface.circle(surfaceId, Vector2D(posX, posY), radius, rotation).value
    world.createSurface
      .expects(SaveSurfaceCommand(surface))
      .returning(Right(Scene(surfaces = Map(surface.id -> surface))))
      .once()
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.createCircleSurface(
      surfaceId,
      posX,
      posY,
      radius,
      rotation = java.lang.Double.valueOf(rotation)
    )

    result shouldBe s"Success: Surface '$surfaceId' created."

  test("when create rectangle surface receives a rotation it is saved"):
    val surface =
      Surface.rectangle(surfaceId, Vector2D(posX, posY), height, rectangleLength, rotation).value
    world.createSurface
      .expects(SaveSurfaceCommand(surface))
      .returning(Right(Scene(surfaces = Map(surface.id -> surface))))
      .once()
    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.createRectangleSurface(
      surfaceId,
      posX,
      posY,
      height,
      rectangleLength,
      rotation = java.lang.Double.valueOf(rotation)
    )

    result shouldBe s"Success: Surface '$surfaceId' created."

  test("when remove surface is called delegates returns a success message"):
    val id = LocatableId(surfaceId).value
    world.removeSurface.expects(id.value).returning(Right(Scene())).once()

    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.removeSurface(surfaceId)

    result shouldBe s"Success: Surface '$surfaceId' removed."

  test("list all teams in the world returns empty if scene is empty"):
    (() => world.getAllTeams).expects().returning(List.empty).once()

    val result = tools.getAllTeams

    result shouldBe "Result: no teams found."

  test("list all teams in the world returns the list"):
    val team = Team.create("blue", Set("red", "green")).value
    (() => world.getAllTeams).expects().returning(List(team)).once()

    val result = tools.getAllTeams

    result shouldBe
      """Result: 1 teams found.
        |1:
        |id: blue
        |enemies: green, red""".stripMargin

  test("when get team is called returns the formatted team"):
    val team = Team.create("blue", Set("red")).value
    world.getTeam.expects(TeamId("blue").value.value).returning(Right(team)).once()

    val result = tools.getTeam("blue")

    result shouldBe
      """Result:
        |id: blue
        |enemies: red""".stripMargin

  test("when create team is called returns a success message"):
    val team = Team.create("blue", Set("red", "green")).value
    world.createTeam
      .expects(SaveTeamCommand(team))
      .returning(Right(Scene(teams = Map(team.id -> team))))
      .once()

    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.createTeam("blue", "red, green")

    result shouldBe "Success: Team 'blue' created."

  test("when update team is called returns a success message"):
    val team = Team.create("blue", Set("yellow")).value
    world.updateTeam
      .expects(SaveTeamCommand(team))
      .returning(Right(Scene(teams = Map(team.id -> team))))
      .once()

    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.updateTeam("blue", "yellow")

    result shouldBe "Success: Team 'blue' updated."

  test("when remove team is called returns a success message"):
    val id = TeamId("blue").value
    world.removeTeam.expects(id.value).returning(Right(Scene())).once()

    (() => gameEngineRuntime.isRunning).expects().returning(false).once()

    val result = tools.removeTeam("blue")

    result shouldBe "Success: Team 'blue' removed."

  test("can not call tools that modify the World while engine is running"):
    (() => gameEngineRuntime.isRunning).expects().returning(true).once()

    tools.createCircleEntity(entityId, posX, posY, radius) shouldBe
      "Error: The world cannot be modified while the game engine is running."

  test("when start is called starts the game engine and return a message"):
    (() => gameEngineRuntime.start()).expects().returning(()).once()

    val result = tools.start()

    result shouldBe "Game engine started."

  test("when stop is called stops the game engine and returns a message"):
    (() => gameEngineRuntime.stop()).expects().returning(()).once()

    val result = tools.stop()

    result shouldBe "Game engine stopped."
