package engine.core

import engine.model.{Entity, Team, Vector2D}
import org.scalatest.{EitherValues, Inside}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SceneTest extends AnyFunSuite with Inside with Matchers with EitherValues:

  val scene = Scene()
  val genericEntityEither: Either[String, Entity] = Entity.circle("Id", Vector2D(0, 0), 1)
  val genericTeamEither: Either[String, Team] = Team.create("id", Set.empty)

  test("A Scene upon creation has all the maps empty"):
    scene.entities.size should be(0)
    scene.surfaces.size should be(0)
    scene.teams.size should be(0)

  test("An entity can be added to the scene"):
    val entity = genericEntityEither.value

    val newSceneEither = scene addEntity entity

    inside(newSceneEither):
      case Right(updatedScene) =>
        updatedScene.entities should contain key entity.id
        updatedScene.entities should contain value entity
        updatedScene.entities.size should be(1)

  test("Adding an entity with the same id as another entity already present in the scene returns an error"):
    val entity = genericEntityEither.value

    val newScene = (scene addEntity entity).value

    inside(newScene addEntity entity):
      case Left(message) => message should be(s"Element with ${entity.id} already present")

  test("A fetch of an unknown entity id returns and error string"):
    val entity = genericEntityEither.value

    inside(scene getEntity entity.id):
      case Left(message) => message should be("Entity Not Found")

  test("An added entity can be get from the scene"):
    val entity = genericEntityEither.value
    val newScene = (scene addEntity entity).value

    val fetchedResult = newScene getEntity entity.id

    inside(fetchedResult):
      case Right(fetchedEntity) =>
        fetchedEntity should be(entity)

  test("An added entity can be remove from the scene"):
    val entity = genericEntityEither.value
    val newScene = (scene addEntity entity).value

    val removeResult = newScene removeEntity entity.id
    val errorFetch = removeResult.value getEntity entity.id

    inside(removeResult):
      case Right(updatedScene) =>
        updatedScene.entities.size should be(0)
        inside(updatedScene getEntity entity.id):
          case Left(message) => message should be("Entity Not Found")


  test("Trying to remove a non present entity returns an error"):
    val entity = genericEntityEither.value

    val removeResult = scene removeEntity entity.id

    inside(removeResult):
      case Left(message) =>
        message should be(s"Element with ${entity.id} not present")

  test("A Team can be added to the scene"):
    val team = genericTeamEither.value

    val newSceneEither = scene addTeam team

    inside(newSceneEither):
      case Right(updatedScene) =>
        updatedScene.teams should contain key team.id
        updatedScene.teams should contain value team
        updatedScene.teams.size should be(1)

  test("Adding a team with the same id as another team already present in the scene returns an error"):
    val team = genericTeamEither.value

    val newScene = (scene addTeam team).value

    inside(newScene addTeam team):
      case Left(message) => message should be(s"Element with ${team.id} already present")

  test("A fetch of an unknown team id returns and error string"):
    val team = genericTeamEither.value

    inside(scene getTeam team.id):
      case Left(message) => message should be("Team Not Found")

  test("An added team can be get from the scene"):
    val team = genericTeamEither.value

    val newScene = (scene addTeam team).value

    inside(newScene getTeam team.id):
      case Right(fetchedEntity) =>
        fetchedEntity should be(team)

  test("An added team can be remove from the scene"):
    val team = genericTeamEither.value
    val newScene = (scene addTeam team).value

    val removeResult = newScene removeTeam team.id
    val errorFetch = removeResult.value getTeam team.id

    inside(removeResult):
      case Right(updatedScene) =>
        updatedScene.teams.size should be(0)
        inside(updatedScene getTeam team.id):
          case Left(message) => message should be("Team Not Found")

  test("Trying to remove a non present team returns an error"):
    val entity = genericEntityEither.value

    val removeResult = scene removeEntity entity.id

    inside(removeResult):
      case Left(message) =>
        message should be(s"Element with ${entity.id} not present")