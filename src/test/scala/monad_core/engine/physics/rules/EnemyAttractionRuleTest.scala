package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.model.*
import monad_core.engine.physics.core.*
import monad_core.engine.physics.helper.PhysicsConstantHelper.*
import monad_core.engine.physics.helper.PhysicsEntityHelper.*
import monad_core.engine.physics.helper.PhysicsTeamHelper.*
import monad_core.engine.physics.helper.{PhysicsDetectorHelper, PhysicsSceneHelper}
import monad_core.engine.physics.pathfinding.{RayCast, VertexFinder}
import monad_core.engine.physics.rules.EnemyAttractionRule
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EnemyAttractionRuleTest
    extends AnyFunSuite
    with Matchers
    with MockFactory
    with PhysicsDetectorHelper
    with PhysicsSceneHelper:

  private val Rule = EnemyAttractionRule.enemyAttractionRule

  private val AttractionAcceleration = 1.0

  private val Epsilon = 1e-12

  private val MockScene   = mock[State]
  given CollisionDetector = mock[CollisionDetector]

  private def calculateRayCastSpeed(
      entity: Entity,
      enemy: Entity,
      allEntities: List[Entity]
  ): Vector2D =

    val targetPos = RayCast(
      to = enemy,
      from = entity,
      entitiesVertexes = VertexFinder.apply(allEntities),
      entities = allEntities,
      upperLeftSceneCorner = MockScene.UpperLeftCorner,
      lowerRightSceneCorner = MockScene.LowerRightCorner
    )

    val direction = (targetPos.value.value - entity.position).normalized
    entity.speed.value + direction * AttractionAcceleration

  test("the rule should return NegativeDeltaTime when delta time is negative"):

    val result = Rule.apply(MockScene, NegativeDt)(using summon[CollisionDetector])

    result shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("the rule should return the unchanged scene when there are no entities"):
    val scene = sceneWithTeams(List(), List())

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    result shouldBe scene

  test("the rule should not update an entity when it has no enemy"):

    val entity = addTeam(
      makeMovingEntityCircle(),
      "teamA"
    )

    val entityTeam = makeTeam(
      id = entity.teamId.value.value,
      enemies = Set("teamB")
    )

    val scene = sceneWithTeams(List(entity), List(entityTeam))

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    val resultEntity = result.allEntities.find(_.id == entity.id).value

    resultEntity.position shouldBe entity.position
    resultEntity.speed shouldBe entity.speed

  test("the rule should not update a fixed entity even when an enemy exists"):

    val fixedEntity = addTeam(
      makeFixedEntityCircle(
        id = "fixed"
      ),
      "teamA"
    )

    val enemy = addTeam(
      makeFixedEntityCircle(
        id = "enemy"
      ),
      "teamB"
    )

    val fixedEntityTeam = makeTeam(fixedEntity.teamId.value.value, Set(enemy.teamId.value.value))
    val enemyTeam       = makeTeam(enemy.teamId.value.value)

    val scene = sceneWithTeams(List(fixedEntity, enemy), List(fixedEntityTeam, enemyTeam))

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    val resultEntity = result.allEntities.find(_.id == fixedEntity.id).value

    resultEntity.speed shouldBe fixedEntity.speed

  test("the rule should accelerate a mobile entity toward its nearest enemy"):

    val entity = addTeam(
      makeMovingEntityCircle(
        position = Vector2D(0, 0),
        speed = Vector2D(2, 2)
      ),
      "teamA"
    )

    val enemy = addTeam(
      makeFixedEntityCircle(
        id = "enemy",
        position = Vector2D(10, 10),
        radius = 1.0
      ),
      "teamB"
    )

    val entityTeam = makeTeam(entity.teamId.value.value, Set(enemy.teamId.value.value))
    val enemyTeam  = makeTeam(enemy.teamId.value.value)

    val expectedSpeed = calculateRayCastSpeed(entity, enemy, List(entity, enemy))

    val scene = sceneWithTeams(List(entity, enemy), List(entityTeam, enemyTeam))

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state
    val resultEntity = result.allEntities.find(_.id == entity.id).value

    resultEntity.speed.value shouldBe expectedSpeed

  test("the rule should update every mobile entity that has an enemy"):

    val entity1 = addTeam(
      makeMovingEntityCircle(
        id = "entity1",
        position = Vector2D(0.0, 0.0),
        speed = Vector2D(1.0, 0.0)
      ),
      "teamA"
    )

    val entity2 = addTeam(
      makeMovingEntityCircle(
        id = "entity2",
        position = Vector2D(5.0, 5.0),
        speed = Vector2D(0.0, 1.0)
      ),
      "teamA"
    )

    val enemy = addTeam(
      makeMovingEntityCircle(
        id = "enemy",
        position = Vector2D(10.0, 10.0)
      ),
      "teamB"
    )

    val entityTeam = makeTeam(entity1.teamId.value.value, Set(enemy.teamId.value.value))
    val enemyTeam  = makeTeam(enemy.teamId.value.value)

    val expectedSpeed1 = calculateRayCastSpeed(entity1, enemy, List(entity1, entity2, enemy))
    val expectedSpeed2 = calculateRayCastSpeed(entity2, enemy, List(entity1, entity2, enemy))

    val scene = sceneWithTeams(List(entity1, entity2, enemy), List(entityTeam, enemyTeam))

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state
    val resultEntity1 = result.allEntities.find(_.id == entity1.id).value
    val resultEntity2 = result.allEntities.find(_.id == entity2.id).value

    resultEntity1.speed.value.x shouldBe expectedSpeed1.x +- Epsilon
    resultEntity1.speed.value.y shouldBe expectedSpeed1.y +- Epsilon
    resultEntity2.speed.value.x shouldBe expectedSpeed2.x +- Epsilon
    resultEntity2.speed.value.y shouldBe expectedSpeed2.y +- Epsilon
