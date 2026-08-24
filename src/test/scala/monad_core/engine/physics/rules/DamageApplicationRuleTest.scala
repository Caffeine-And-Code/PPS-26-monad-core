package monad_core.engine.physics.rules

import monad_core.engine.helper.PhysicsConstantHelper.{DeltaTimeOneSecond, NegativeDt}
import monad_core.engine.model.*
import monad_core.engine.physics.core.*
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DamageApplicationRuleTest extends AnyFunSuite with Matchers:

  private val Rule = DamageApplicationRule.damageApplicationRule

  private val ContactCollision = monad_core.engine.geometry.Collision(
    normalVector = Vector2D(1, 0),
    penetrationDepth = 1,
    collisionPoint = Vector2D(0.5, 0)
  )

  test("the rule should apply entity damage in both directions"):
    val first  = entityWithStats("first", health = 20, damage = 5)
    val second = entityWithStats("second", health = 20, damage = 7)
    val state  = sceneWith(first, second)
    val context = PhysicsContext(
      state,
      DeltaTimeOneSecond,
      CollisionSnapshot(
        entityContacts = Vector(EntityCollisionContact(first.id, second.id, ContactCollision))
      )
    )

    val result = Rule(context).value.state

    healthOf(result, first) shouldBe 13
    healthOf(result, second) shouldBe 15

  test("the rule should compose entity and surface damage from the same snapshot"):
    val first   = entityWithStats("first", health = 20, damage = 2)
    val second  = entityWithStats("second", health = 20, damage = 3)
    val surface = Surface.circle("surface", Vector2D(0, 0), 10).value.withDamageOverTime(4).value
    val state = Scene(
      entities = Map(first.id -> first, second.id -> second),
      surfaces = Map(surface.id -> surface)
    )
    val context = PhysicsContext(
      state,
      DeltaTimeOneSecond,
      CollisionSnapshot(
        entityContacts = Vector(EntityCollisionContact(first.id, second.id, ContactCollision)),
        surfaceContacts = Vector(SurfaceContact(first.id, surface.id))
      )
    )

    val result = Rule(context).value.state

    healthOf(result, first) shouldBe 13
    healthOf(result, second) shouldBe 18

  test("the rule should ignore damage when the target has no health"):
    val entity  = Entity.circle("entity", Vector2D(0, 0), 1).value
    val surface = Surface.circle("surface", Vector2D(0, 0), 10).value.withDamageOverTime(4).value
    val state = Scene(
      entities = Map(entity.id -> entity),
      surfaces = Map(surface.id -> surface)
    )
    val context = PhysicsContext(
      state,
      DeltaTimeOneSecond,
      CollisionSnapshot(surfaceContacts = Vector(SurfaceContact(entity.id, surface.id)))
    )

    Rule(context).value.state shouldBe state

  test("the rule should remove an entity when damage depletes its health"):
    val target   = entityWithStats("target", health = 3, damage = 0)
    val attacker = entityWithStats("attacker", health = 20, damage = 3)
    val state    = sceneWith(target, attacker)
    val context = PhysicsContext(
      state,
      DeltaTimeOneSecond,
      CollisionSnapshot(
        entityContacts = Vector(EntityCollisionContact(target.id, attacker.id, ContactCollision))
      )
    )

    val result = Rule(context).value.state

    result.allEntities.map(_.id) should not contain target.id
    healthOf(result, attacker) shouldBe 20

  test("the rule should remove an entity killed by surface damage"):
    val entity  = entityWithStats("entity", health = 4, damage = 0)
    val surface = Surface.circle("surface", Vector2D(0, 0), 10).value.withDamageOverTime(5).value
    val state = Scene(
      entities = Map(entity.id -> entity),
      surfaces = Map(surface.id -> surface)
    )
    val context = PhysicsContext(
      state,
      DeltaTimeOneSecond,
      CollisionSnapshot(surfaceContacts = Vector(SurfaceContact(entity.id, surface.id)))
    )

    Rule(context).value.state.allEntities shouldBe empty

  test("the rule should reject a negative delta time"):
    Rule(PhysicsContext(Scene(), NegativeDt)) shouldBe Left(NegativeDeltaTime(NegativeDt))

  private def entityWithStats(id: String, health: Int, damage: Int): Entity =
    (for
      entity     <- Entity.circle(id, Vector2D(0, 0), 1)
      withHealth <- entity.withHealth(health)
      withDamage <- withHealth.withDamage(damage)
    yield withDamage).value

  private def sceneWith(entities: Entity*): Scene =
    Scene(entities = entities.map(entity => entity.id -> entity).toMap)

  private def healthOf(state: monad_core.engine.core.traits.State, entity: Entity): Int =
    state.getEntity(entity.id).value.health.value.value
