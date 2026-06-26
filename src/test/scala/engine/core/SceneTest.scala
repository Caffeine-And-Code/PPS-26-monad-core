package engine.core

import engine.model.{Entity, Surface, Team, Vector2D}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, Inside}

class SceneTest extends AnyFunSuite with Inside with Matchers with EitherValues:

  val InitializedScene = Scene()
  val GenericEntity: Entity = Entity.circle("Id", Vector2D(0, 0), 1).value
  val GenericTeam: Team = Team.create("id", Set.empty).value
  val GenericSurface: Surface = Surface.circle("Id", Vector2D(0, 0), 1).value

  def addAnElementToEachMap(): Scene =
    InitializedScene
      .addEntity(GenericEntity).value
      .addTeam(GenericTeam).value
      .addSurface(GenericSurface).value

  test("A Scene upon creation has all the maps empty"):
    InitializedScene.entities.size should be(0)
    InitializedScene.surfaces.size should be(0)
    InitializedScene.teams.size should be(0)

  test("An entity can be added to the scene"):
    val newSceneEither = InitializedScene addEntity GenericEntity

    inside(newSceneEither):
      case Right(updatedScene) =>
        updatedScene.entities should contain key GenericEntity.id
        updatedScene.entities should contain value GenericEntity
        updatedScene.entities.size should be(1)

  test("Adding an entity with the same id as another entity already present in the scene returns an error"):
    val newScene = (InitializedScene addEntity GenericEntity).value
    val expectedError = CannotAddEntity(CannotAddAlreadyPresentElementInMap(GenericEntity.id))

    val addResult = newScene addEntity GenericEntity

    inside(addResult):
      case Left(message) =>
        message should be(expectedError)

  test("A fetch of an unknown entity id returns and error string"):
    val fetchResult = InitializedScene getEntity GenericEntity.id

    inside(fetchResult):
      case Left(message) => message should be(EntityNotFound(GenericEntity.id))

  test("An added entity can be get from the scene"):
    val newScene = (InitializedScene addEntity GenericEntity).value

    val fetchedResult = newScene getEntity GenericEntity.id

    inside(fetchedResult):
      case Right(fetchedEntity) =>
        fetchedEntity should be(GenericEntity)

  test("An added entity can be remove from the scene"):
    val newScene = (InitializedScene addEntity GenericEntity).value

    val removeResult = newScene removeEntity GenericEntity.id
    val errorFetch = removeResult.value getEntity GenericEntity.id

    inside(removeResult):
      case Right(updatedScene) =>
        updatedScene.entities.size should be(0)
        inside(updatedScene getEntity GenericEntity.id):
          case Left(message) => message should be(EntityNotFound(GenericEntity.id))


  test("Trying to remove a non present entity returns an error"):
    val expectedError = CannotRemoveEntity(CannotRemoveNonPresentElementFromMap(GenericEntity.id))
    val removeResult = InitializedScene removeEntity GenericEntity.id

    inside(removeResult):
      case Left(message) =>
        message should be(expectedError)

  test("A Team can be added to the scene"):
    val newSceneEither = InitializedScene addTeam GenericTeam

    inside(newSceneEither):
      case Right(updatedScene) =>
        updatedScene.teams should contain key GenericTeam.id
        updatedScene.teams should contain value GenericTeam
        updatedScene.teams.size should be(1)

  test("Adding a team with the same id as another team already present in the scene returns an error"):
    val expectedError = CannotAddTeam(CannotAddAlreadyPresentElementInMap(GenericTeam.id))
    val newScene = (InitializedScene addTeam GenericTeam).value

    val addResult = newScene addTeam GenericTeam

    inside(addResult):
      case Left(message) => message should be(expectedError)

  test("A fetch of an unknown team id returns and error string"):
    val fetchResult = InitializedScene getTeam GenericTeam.id

    inside(fetchResult):
      case Left(message) => message should be(TeamNotFound(GenericTeam.id))

  test("An added team can be get from the scene"):
    val newScene = (InitializedScene addTeam GenericTeam).value

    inside(newScene getTeam GenericTeam.id):
      case Right(fetchedEntity) =>
        fetchedEntity should be(GenericTeam)

  test("An added team can be remove from the scene"):
    val newScene = (InitializedScene addTeam GenericTeam).value

    val removeResult = newScene removeTeam GenericTeam.id
    val errorFetch = removeResult.value getTeam GenericTeam.id

    inside(removeResult):
      case Right(updatedScene) =>
        updatedScene.teams.size should be(0)
        inside(updatedScene getTeam GenericTeam.id):
          case Left(message) => message should be(TeamNotFound(GenericTeam.id))

  test("Trying to remove a non present team returns an error"):
    val expectedError = CannotRemoveTeam(CannotRemoveNonPresentElementFromMap(GenericTeam.id))

    val removeResult = InitializedScene removeTeam GenericTeam.id

    inside(removeResult):
      case Left(message) =>
        message should be(expectedError)

  test("A Surface can be added to the scene"):
    val newSceneEither = InitializedScene addSurface GenericSurface

    inside(newSceneEither):
      case Right(updatedScene) =>
        updatedScene.surfaces should contain key GenericSurface.id
        updatedScene.surfaces should contain value GenericSurface
        updatedScene.surfaces.size should be(1)

  test("Adding a surface with the same id as another surface already present in the scene returns an error"):
    val expectedError = CannotAddSurface(CannotAddAlreadyPresentElementInMap(GenericSurface.id))
    val newScene = (InitializedScene addSurface GenericSurface).value

    val addResult = newScene addSurface GenericSurface

    inside(addResult):
      case Left(message) => message should be(expectedError)

  test("A fetch of an unknown surface id returns and error string"):
    val fetchResult = InitializedScene getSurface GenericSurface.id

    inside(fetchResult):
      case Left(message) => message should be(SurfaceNotFound(GenericSurface.id))

  test("An added surface can be get from the scene"):
    val newScene = (InitializedScene addSurface GenericSurface).value

    val fetchResult = newScene getSurface GenericSurface.id

    inside(fetchResult):
      case Right(fetchedEntity) =>
        fetchedEntity should be(GenericSurface)

  test("An added surface can be remove from the scene"):
    val newScene = (InitializedScene addSurface GenericSurface).value

    val removeResult = newScene removeSurface GenericSurface.id
    val errorFetch = removeResult.value getSurface GenericSurface.id

    inside(removeResult):
      case Right(updatedScene) =>
        updatedScene.surfaces.size should be(0)
        inside(updatedScene getSurface GenericSurface.id):
          case Left(message) => message should be(SurfaceNotFound(GenericSurface.id))

  test("Trying to remove a non present surface returns an error"):
    val expectedError = CannotRemoveSurface(CannotRemoveNonPresentElementFromMap(GenericSurface.id))

    val removeResult = InitializedScene removeSurface GenericSurface.id

    inside(removeResult):
      case Left(message) =>
        message should be(expectedError)

  test("Adding an entity doesn't effect the surfaces and teams"):
    val entityToAdd = Entity.rectangle(
      id = "new entity",
      position = Vector2D(0, 0),
      height = 10,
      length = 10
    ).value
    val testingScene = this.addAnElementToEachMap()

    val addingResult = testingScene addEntity entityToAdd

    inside(addingResult):
      case Right(updatedScene) =>
        updatedScene.surfaces.size should be(1)
        updatedScene.teams.size should be(1)


  test("removing an entity doesn't effect the surfaces and teams"):
    val testingScene = this.addAnElementToEachMap()

    val removeResult = testingScene removeEntity GenericEntity.id

    inside(removeResult):
      case Right(updatedScene) =>
        updatedScene.surfaces.size should be(1)
        updatedScene.teams.size should be(1)

  test("Adding a team doesn't effect the surfaces and entities"):
    val teamToAdd = Team.create("new team", Set.empty).value
    val testingScene = this.addAnElementToEachMap()

    val addingResult = testingScene addTeam teamToAdd

    inside(addingResult):
      case Right(updatedScene) =>
        updatedScene.surfaces.size should be(1)
        updatedScene.entities.size should be(1)


  test("removing a team doesn't effect the surfaces and entities"):
    val testingScene = this.addAnElementToEachMap()

    val removeResult = testingScene removeTeam GenericTeam.id

    inside(removeResult):
      case Right(updatedScene) =>
        updatedScene.surfaces.size should be(1)
        updatedScene.entities.size should be(1)


  test("Adding a surface doesn't effect the teams and entities"):
    val surfaceToAdd = Surface.rectangle(
      id = "new entity",
      position = Vector2D(0, 0),
      height = 10,
      length = 10
    ).value
    val testingScene = this.addAnElementToEachMap()

    val addingResult = testingScene addSurface surfaceToAdd

    inside(addingResult):
      case Right(updatedScene) =>
        updatedScene.teams.size should be(1)
        updatedScene.entities.size should be(1)


  test("removing a surface doesn't effect the teams and entities"):
    val testingScene = this.addAnElementToEachMap()

    val removeResult = testingScene removeSurface GenericSurface.id

    inside(removeResult):
      case Right(updatedScene) =>
        updatedScene.teams.size should be(1)
        updatedScene.entities.size should be(1)