package monad_core.engine.physics.core

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.events.Event
import monad_core.engine.core.events.Event.EntityUpdatedEvent
import monad_core.engine.core.traits.{PhysicsEngine, PhysicsStep, State}
import monad_core.engine.geometry.ShapeCollision.shapeCollidesWithShape
import monad_core.engine.geometry.ShapeContainment.shapeContainsPoint
import monad_core.engine.physics.combinators.RuleCombinator
import monad_core.engine.physics.rules.*

final case class PhysicsManager private (
    rules: Seq[PhysicsRule],
    enabledRules: Set[PhysicsRule]
)(using detector: CollisionDetector)
    extends PhysicsEngine:

  override def step(scene: State, deltaTime: Long): Either[PhysicsError, PhysicsStep] =
    val activeRules = rules.filter(enabledRules.contains)
    RuleCombinator.sequence(activeRules)(scene, deltaTime)(using detector).map { result =>
      PhysicsStep(
        state = result.state,
        events = result.events ++ detectEntityUpdateEvents(scene, result.state)
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

  private def detectEntityUpdateEvents(before: State, after: State): Vector[Event] =
    val previousEntities = before.allEntities.map(entity => entity.id -> entity).toMap
    val currentEntities  = after.allEntities.map(entity => entity.id -> entity).toMap

    (previousEntities.keySet intersect currentEntities.keySet).toVector
      .sortBy(_.value)
      .collect {
        case id if previousEntities(id) != currentEntities(id) =>
          EntityUpdatedEvent(currentEntities(id))
      }

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
        SurfaceDynamicsRule.surfaceDynamicsRule,
        EnemyAttractionRule.enemyAttractionRule,
        CollisionResolutionRule.collisionResolutionRule,
        BorderContactRule.borderContactRule,
        KinematicsRule.kinematicsRule
      )
    )
