package monad_core.engine.core

import monad_core.engine.core.{
  CannotAddAlreadyPresentElementInMap,
  CannotAddEntity,
  CannotAddSurface,
  CannotAddTeam,
  CannotRemoveEntity,
  CannotRemoveNonPresentElementFromMap,
  CannotRemoveSurface,
  CannotRemoveTeam,
  EntityNotFound,
  SurfaceNotFound,
  TeamNotFound
}
import monad_core.engine.model.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, Inside}

class SceneTest extends AnyFunSuite with Inside with Matchers with EitherValues:

  val InitializedScene        = Scene()
  val GenericEntity: Entity   = Entity.circle("Id", Vector2D(0, 0), 1).value
  val GenericTeam: Team       = Team.create("id", Set.empty).value
  val GenericSurface: Surface = Surface.circle("Id", Vector2D(0, 0), 1).value

  def addAnElementToEachMap(): Either[EngineError, Scene] =
    for
      s1 <- InitializedScene.addEntity(GenericEntity)
      s2 <- s1.addTeam(GenericTeam)
      s3 <- s2.addSurface(GenericSurface)
    yield s3

  test("A Scene upon creation has all the maps empty"):
    InitializedScene.entities.size should be(0)
    InitializedScene.surfaces.size should be(0)
    InitializedScene.teams.size should be(0)

  test("An entity can be added to the scene"):
    val newSceneEither = InitializedScene.addEntity(GenericEntity)

    inside(newSceneEither):
      case Right(updatedScene) =>
        updatedScene.entities should contain key GenericEntity.id
        updatedScene.entities should contain value GenericEntity
        updatedScene.entities.size should be(1)

  test(
    "Adding an entity with the same id as another entity already present in the scene returns an error"
  ):
    val expectedError = CannotAddEntity(CannotAddAlreadyPresentElementInMap(GenericEntity.id))

    val result = for
      newScene  <- InitializedScene.addEntity(GenericEntity)
      addResult <- newScene.addEntity(GenericEntity)
    yield addResult

    inside(result):
      case Left(message) =>
        message should be(expectedError)

  test("A fetch of an unknown entity id returns and error string"):
    val fetchResult = InitializedScene.getEntity(GenericEntity.id)

    inside(fetchResult):
      case Left(message) => message should be(EntityNotFound(GenericEntity.id))

  test("An added entity can be get from the scene"):
    val result = for
      newScene  <- InitializedScene.addEntity(GenericEntity)
      getResult <- newScene.getEntity(GenericEntity.id)
    yield getResult

    inside(result):
      case Right(fetchedEntity) =>
        fetchedEntity should be(GenericEntity)

  test("An added entity can be removed from the scene"):
    val result = for
      sceneWithEntity    <- InitializedScene.addEntity(GenericEntity)
      sceneWithoutEntity <- sceneWithEntity.removeEntity(GenericEntity)
    yield sceneWithoutEntity

    inside(result):
      case Right(updatedScene) =>
        updatedScene.entities.size should be(0)
        updatedScene.getEntity(GenericEntity.id) should be(Left(EntityNotFound(GenericEntity.id)))

  test("Trying to remove a non present entity returns an error"):
    val expectedError = CannotRemoveEntity(CannotRemoveNonPresentElementFromMap(GenericEntity.id))
    val removeResult  = InitializedScene.removeEntity(GenericEntity)

    inside(removeResult):
      case Left(message) =>
        message should be(expectedError)

  test("A Team can be added to the scene"):
    val newSceneEither = InitializedScene.addTeam(GenericTeam)

    inside(newSceneEither):
      case Right(updatedScene) =>
        updatedScene.teams should contain key GenericTeam.id
        updatedScene.teams should contain value GenericTeam
        updatedScene.teams.size should be(1)

  test(
    "Adding a team with the same id as another team already present in the scene returns an error"
  ):
    val expectedError = CannotAddTeam(CannotAddAlreadyPresentElementInMap(GenericTeam.id))

    val result = for
      sceneWithTeam <- InitializedScene.addTeam(GenericTeam)
      error         <- sceneWithTeam.addTeam(GenericTeam)
    yield error

    inside(result):
      case Left(message) => message should be(expectedError)

  test("A fetch of an unknown team id returns and error string"):
    val fetchResult = InitializedScene.getTeam(GenericTeam.id)

    inside(fetchResult):
      case Left(message) => message should be(TeamNotFound(GenericTeam.id))

  test("An added team can be get from the scene"):
    val result = for
      sceneWithTeam <- InitializedScene.addTeam(GenericTeam)
      fetchResult   <- sceneWithTeam.getTeam(GenericTeam.id)
    yield fetchResult

    inside(result):
      case Right(fetchedEntity) =>
        fetchedEntity should be(GenericTeam)

  test("An added team can be remove from the scene"):
    val result = for
      sceneWithTeam    <- InitializedScene.addTeam(GenericTeam)
      sceneWithoutTeam <- sceneWithTeam.removeTeam(GenericTeam)
    yield sceneWithoutTeam

    inside(result):
      case Right(updatedScene) =>
        updatedScene.teams.size should be(0)
        updatedScene.getTeam(GenericTeam.id) should be(Left(TeamNotFound(GenericTeam.id)))

  test("Trying to remove a non present team returns an error"):
    val expectedError = CannotRemoveTeam(CannotRemoveNonPresentElementFromMap(GenericTeam.id))

    val removeResult = InitializedScene.removeTeam(GenericTeam)

    inside(removeResult):
      case Left(message) =>
        message should be(expectedError)

  test("A Surface can be added to the scene"):
    val newSceneEither = InitializedScene.addSurface(GenericSurface)

    inside(newSceneEither):
      case Right(updatedScene) =>
        updatedScene.surfaces should contain key GenericSurface.id
        updatedScene.surfaces should contain value GenericSurface
        updatedScene.surfaces.size should be(1)

  test(
    "Adding a surface with the same id as another surface already present in the scene returns an error"
  ):
    val expectedError = CannotAddSurface(CannotAddAlreadyPresentElementInMap(GenericSurface.id))

    val addResult = for
      sceneWithSurface <- InitializedScene.addSurface(GenericSurface)
      error            <- sceneWithSurface.addSurface(GenericSurface)
    yield error

    inside(addResult):
      case Left(message) => message should be(expectedError)

  test("A fetch of an unknown surface id returns and error string"):
    val fetchResult = InitializedScene.getSurface(GenericSurface.id)

    inside(fetchResult):
      case Left(message) => message should be(SurfaceNotFound(GenericSurface.id))

  test("An added surface can be get from the scene"):
    val fetchResult = for
      sceneWithSurface <- InitializedScene.addSurface(GenericSurface)
      fetchRes         <- sceneWithSurface.getSurface(GenericSurface.id)
    yield fetchRes

    inside(fetchResult):
      case Right(fetchedEntity) =>
        fetchedEntity should be(GenericSurface)

  test("An added surface can be remove from the scene"):
    val result = for
      sceneWithSurface    <- InitializedScene.addSurface(GenericSurface)
      sceneWithoutSurface <- sceneWithSurface.removeSurface(GenericSurface)
    yield sceneWithoutSurface

    inside(result):
      case Right(updatedScene) =>
        updatedScene.surfaces.size should be(0)
        updatedScene.getSurface(GenericSurface.id) should be(
          Left(SurfaceNotFound(GenericSurface.id))
        )

  test("Trying to remove a non present surface returns an error"):
    val expectedError = CannotRemoveSurface(CannotRemoveNonPresentElementFromMap(GenericSurface.id))

    val removeResult = InitializedScene.removeSurface(GenericSurface)

    inside(removeResult):
      case Left(message) =>
        message should be(expectedError)

  test("Adding an entity doesn't effect the surfaces and teams"):
    val entityToAdd = Entity
      .rectangle(
        id = "new entity",
        position = Vector2D(0, 0),
        height = 10,
        length = 10
      )
      .value

    val addingResult = for
      sceneWithElements <- this.addAnElementToEachMap()
      resultScene       <- sceneWithElements.addEntity(entityToAdd)
    yield resultScene

    inside(addingResult):
      case Right(updatedScene) =>
        updatedScene.surfaces.size should be(1)
        updatedScene.teams.size should be(1)

  test("removing an entity doesn't effect the surfaces and teams"):
    val removeResult = for
      sceneWithElements <- this.addAnElementToEachMap()
      resultScene       <- sceneWithElements.removeEntity(GenericEntity)
    yield resultScene

    inside(removeResult):
      case Right(updatedScene) =>
        updatedScene.surfaces.size should be(1)
        updatedScene.teams.size should be(1)

  test("Adding a team doesn't effect the surfaces and entities"):
    val teamToAdd = Team.create("new team", Set.empty).value

    val addingResult = for
      sceneWithElements <- this.addAnElementToEachMap()
      resultScene       <- sceneWithElements.addTeam(teamToAdd)
    yield resultScene

    inside(addingResult):
      case Right(updatedScene) =>
        updatedScene.surfaces.size should be(1)
        updatedScene.entities.size should be(1)

  test("removing a team doesn't effect the surfaces and entities"):
    val removeResult = for
      sceneWithElements <- this.addAnElementToEachMap()
      resultScene       <- sceneWithElements.removeTeam(GenericTeam)
    yield resultScene

    inside(removeResult):
      case Right(updatedScene) =>
        updatedScene.surfaces.size should be(1)
        updatedScene.entities.size should be(1)

  test("Adding a surface doesn't effect the teams and entities"):
    val surfaceToAdd = Surface
      .rectangle(
        id = "new entity",
        position = Vector2D(0, 0),
        height = 10,
        length = 10
      )
      .value

    val addingResult = for
      sceneWithElements <- this.addAnElementToEachMap()
      resultScene       <- sceneWithElements.addSurface(surfaceToAdd)
    yield resultScene

    inside(addingResult):
      case Right(updatedScene) =>
        updatedScene.teams.size should be(1)
        updatedScene.entities.size should be(1)

  test("removing a surface doesn't effect the teams and entities"):
    val removeResult = for
      sceneWithElements <- this.addAnElementToEachMap()
      resultScene       <- sceneWithElements.removeSurface(GenericSurface)
    yield resultScene

    inside(removeResult):
      case Right(updatedScene) =>
        updatedScene.teams.size should be(1)
        updatedScene.entities.size should be(1)

  test("allEntities actually retrieve all the entities of the scene"):
    val secondGenericEntity = Entity.circle("SecondId", Vector2D(0, 0), 1).value
    val arrangedScene =
      (for
        intermediateScene <- InitializedScene.addEntity(GenericEntity)
        finalScene        <- intermediateScene.addEntity(secondGenericEntity)
      yield finalScene).value

    val allFetchResult = arrangedScene.allEntities

    allFetchResult.length should be(2)
    allFetchResult should contain(GenericEntity)
    allFetchResult should contain(secondGenericEntity)

  test("allTeams actually retrieve all the teams of the scene"):
    val secondGenericTeam = Team.apply(TeamId("SecondId").value).value
    val arrangedScene =
      (for
        intermediateScene <- InitializedScene.addTeam(GenericTeam)
        finalScene        <- intermediateScene.addTeam(secondGenericTeam)
      yield finalScene).value

    val allFetchResult = arrangedScene.allTeams

    allFetchResult.length should be(2)
    allFetchResult should contain(GenericTeam)
    allFetchResult should contain(secondGenericTeam)

  test("allSurfaces actually retrieve all the surfaces of the scene"):
    val secondGenericSurface = Surface.circle("SecondId", Vector2D(0, 0), 1).value
    val arrangedScene =
      (for
        intermediateScene <- InitializedScene.addSurface(GenericSurface)
        finalScene        <- intermediateScene.addSurface(secondGenericSurface)
      yield finalScene).value

    val allFetchResult = arrangedScene.allSurfaces

    allFetchResult.length should be(2)
    allFetchResult should contain(GenericSurface)
    allFetchResult should contain(secondGenericSurface)

  test("Scene bounds are correctly set on default upon creation"):
    val scene = Scene()

    scene.bounds.upperLeft should be(Vector2D(0, 0))
    scene.bounds.lowerRight should be(Vector2D(100, 100))

  test("Scene bounds can be correctly set upon creation"):
    val customBounds = WorldBounds(50, 100).value
    val scene        = Scene(bounds = customBounds)

    scene.bounds.upperLeft should be(Vector2D(0, 0))
    scene.bounds.lowerRight should be(Vector2D(50, 100))