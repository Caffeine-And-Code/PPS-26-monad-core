package monad_core.engine.physics.core

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.events.EngineEvent
import monad_core.engine.core.events.EngineEvent.{EntityRemoved, EntityUpdated}
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
    val context     = PhysicsContext.detect(scene, deltaTime)(using detector)

    RuleCombinator.sequence(activeRules)(context).map { result =>
      PhysicsStep(
        state = result.state,
        events = result.events ++ detectEntityStateEvents(scene, result.state)
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

  private def detectEntityStateEvents(before: State, after: State): Vector[EngineEvent] =
    val previousEntities = before.allEntities.map(entity => entity.id -> entity).toMap
    val currentEntities  = after.allEntities.map(entity => entity.id -> entity).toMap

    previousEntities.keys.toVector
      .sortBy(_.value)
      .flatMap { id =>
        currentEntities.get(id) match
          case None =>
            Some(EntityRemoved(previousEntities(id)))
          case Some(current) if previousEntities(id) != current =>
            Some(EntityUpdated(previousEntities(id), current))
          case _ => None
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
        DamageApplicationRule.damageApplicationRule,
        KinematicsRule.kinematicsRule
      )
    )
