package monad_core.engine.physics.core

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.events.Event
import monad_core.engine.core.events.Event.{
  EntityCollisionDetectedEvent,
  EntityCreatedEvent,
  EntityRemovedEvent,
  EntityUpdatedEvent
}
import monad_core.engine.core.traits.{PhysicsEngine, PhysicsStep, State}
import monad_core.engine.geometry.ShapeCollision.shapeCollidesWithShape
import monad_core.engine.geometry.ShapeContainment.shapeContainsPoint
import monad_core.engine.model.*
import monad_core.engine.physics.combinators.RuleCombinator
import monad_core.engine.physics.rules.*

final case class PhysicsManager private (
    rules: Seq[PhysicsRule],
    enabledRules: Set[PhysicsRule]
)(using detector: CollisionDetector)
    extends PhysicsEngine:

  override def step(scene: State, deltaTime: Long): Either[PhysicsError, PhysicsStep] =
    val activeRules  = rules.filter(enabledRules.contains)
    val updatedScene = RuleCombinator.sequence(activeRules)(scene, deltaTime)(using detector)

    updatedScene.map { nextState =>
      val collisionEvents =
        if activeRules.contains(CollisionResolutionRule.collisionResolutionRule) then
          detectCollisionEvents(scene)
        else Vector.empty

      PhysicsStep(
        state = nextState,
        events = collisionEvents ++ detectEntityEvents(scene, nextState)
      )
    }

  def enable(rule: PhysicsRule): PhysicsManager =
    copy(enabledRules = enabledRules + rule)

  def disable(rule: PhysicsRule): PhysicsManager =
    copy(enabledRules = enabledRules - rule)

  def enableAll: PhysicsManager =
    copy(enabledRules = rules.toSet)

  def disableAll: PhysicsManager =
    copy(enabledRules = Set.empty)

  def isEnabled(rule: PhysicsRule): Boolean =
    enabledRules.contains(rule)

  private def detectCollisionEvents(scene: State): Vector[Event] =
    scene.allEntities
      .sortBy(_.id.value)
      .combinations(n = 2)
      .collect {
        case Seq(first, second) if !(first.isFixed && second.isFixed) =>
          detector
            .collision(first, second)
            .map(collision => EntityCollisionDetectedEvent(first.id, second, collision))
      }
      .flatten
      .toVector

  private def detectEntityEvents(before: State, after: State): Vector[Event] =
    val previousEntities = before.allEntities.map(entity => entity.id -> entity).toMap
    val currentEntities  = after.allEntities.map(entity => entity.id -> entity).toMap

    val removed = (previousEntities.keySet -- currentEntities.keySet).toVector
      .sortBy(_.value)
      .map(id => EntityRemovedEvent(previousEntities(id)))

    val created = (currentEntities.keySet -- previousEntities.keySet).toVector
      .sortBy(_.value)
      .map(id => EntityCreatedEvent(currentEntities(id)))

    val updated = (previousEntities.keySet intersect currentEntities.keySet).toVector
      .sortBy(_.value)
      .collect {
        case id if previousEntities(id) != currentEntities(id) =>
          EntityUpdatedEvent(currentEntities(id))
      }

    removed ++ created ++ updated

object PhysicsManager:

  def apply(
      rules: Vector[PhysicsRule]
  )(using collisionDetector: CollisionDetector): PhysicsManager =
    PhysicsManager(
      rules = rules,
      enabledRules = rules.toSet
    )

  def default(): PhysicsManager =
    apply(
      Vector(
        EnemyAttractionRule.enemyAttractionRule,
        SurfaceDynamicsRule.surfaceDynamicsRule,
        CollisionResolutionRule.collisionResolutionRule,
        BorderContactRule.borderContactRule,
        KinematicsRule.kinematicsRule
      )
    )
