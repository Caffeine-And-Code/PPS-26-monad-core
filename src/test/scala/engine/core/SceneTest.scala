package engine.core

import engine.model.{Entity, Surface, Team, Vector2D}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, Inside}

class SceneTest extends AnyFunSuite with Inside with Matchers with EitherValues:

  val scene = Scene()
  val entity: Entity = Entity.circle("Id", Vector2D(0, 0), 1).value
  val team: Team = Team.create("id", Set.empty).value
  val surface : Surface = Surface.circle("Id", Vector2D(0, 0), 1).value

  test("A Scene upon creation has all the maps empty"):
    scene.entities.size should be(0)
    scene.surfaces.size should be(0)
    scene.teams.size should be(0)

  test("An entity can be added to the scene"):
    val newSceneEither = scene addEntity entity

    inside(newSceneEither):
      case Right(updatedScene) =>
        updatedScene.entities should contain key entity.id
        updatedScene.entities should contain value entity
        updatedScene.entities.size should be(1)

  test("Adding an entity with the same id as another entity already present in the scene returns an error"):
    val newScene = (scene addEntity entity).value

    inside(newScene addEntity entity):
      case Left(message) =>
        message should be(s"Element with key ${entity.id} already present")

  test("A fetch of an unknown entity id returns and error string"):
    val fetchResult = scene getEntity entity.id

    inside(fetchResult):
      case Left(message) => message should be(s"Entity ${entity.id} Not Found")

  test("An added entity can be get from the scene"):
    val newScene = (scene addEntity entity).value

    val fetchedResult = newScene getEntity entity.id

    inside(fetchedResult):
      case Right(fetchedEntity) =>
        fetchedEntity should be(entity)

  test("An added entity can be remove from the scene"):
    val newScene = (scene addEntity entity).value

    val removeResult = newScene removeEntity entity.id
    val errorFetch = removeResult.value getEntity entity.id

    inside(removeResult):
      case Right(updatedScene) =>
        updatedScene.entities.size should be(0)
        inside(updatedScene getEntity entity.id):
          case Left(message) => message should be(s"Entity ${entity.id} Not Found")


  test("Trying to remove a non present entity returns an error"):
    val removeResult = scene removeEntity entity.id

    inside(removeResult):
      case Left(message) =>
        message should be(s"Element with key ${entity.id} not present")

  test("A Team can be added to the scene"):
    val newSceneEither = scene addTeam team

    inside(newSceneEither):
      case Right(updatedScene) =>
        updatedScene.teams should contain key team.id
        updatedScene.teams should contain value team
        updatedScene.teams.size should be(1)

  test("Adding a team with the same id as another team already present in the scene returns an error"):
    val newScene = (scene addTeam team).value

    inside(newScene addTeam team):
      case Left(message) => message should be(s"Element with key ${team.id} already present")

  test("A fetch of an unknown team id returns and error string"):
    val fetchResult = scene getTeam team.id

    inside(fetchResult):
      case Left(message) => message should be(s"Team ${team.id} Not Found")

  test("An added team can be get from the scene"):
    val newScene = (scene addTeam team).value

    inside(newScene getTeam team.id):
      case Right(fetchedEntity) =>
        fetchedEntity should be(team)

  test("An added team can be remove from the scene"):
    val newScene = (scene addTeam team).value

    val removeResult = newScene removeTeam team.id
    val errorFetch = removeResult.value getTeam team.id

    inside(removeResult):
      case Right(updatedScene) =>
        updatedScene.teams.size should be(0)
        inside(updatedScene getTeam team.id):
          case Left(message) => message should be(s"Team ${team.id} Not Found")

  test("Trying to remove a non present team returns an error"):
    val removeResult = scene removeTeam team.id

    inside(removeResult):
      case Left(message) =>
        message should be(s"Element with key ${team.id} not present")

  test("A Surface can be added to the scene"):
    val newSceneEither = scene addSurface surface

    inside(newSceneEither):
      case Right(updatedScene) =>
        updatedScene.surfaces should contain key surface.id
        updatedScene.surfaces should contain value surface
        updatedScene.surfaces.size should be(1)

  test("Adding a surface with the same id as another surface already present in the scene returns an error"):
    val newScene = (scene addSurface surface).value

    val addResult = newScene addSurface surface

    inside(addResult):
      case Left(message) => message should be(s"Element with key ${surface.id} already present")

  test("A fetch of an unknown surface id returns and error string"):
    val fetchResult = scene getSurface surface.id

    inside(fetchResult):
      case Left(message) => message should be(s"Surface ${surface.id} Not Found")

  test("An added surface can be get from the scene"):
    val newScene = (scene addSurface surface).value

    val fetchResult = newScene getSurface surface.id

    inside(fetchResult):
      case Right(fetchedEntity) =>
        fetchedEntity should be(surface)

  test("An added surface can be remove from the scene"):
    val newScene = (scene addSurface  surface).value

    val removeResult = newScene removeSurface  surface.id
    val errorFetch = removeResult.value getSurface  surface.id

    inside(removeResult):
      case Right(updatedScene) =>
        updatedScene.surfaces.size should be(0)
        inside(updatedScene getSurface  surface.id):
          case Left(message) => message should be(s"Surface ${surface.id} Not Found")

  test("Trying to remove a non present surface returns an error"):
    val removeResult = scene removeSurface surface.id

    inside(removeResult):
      case Left(message) =>
        message should be(s"Element with key ${surface.id} not present")
