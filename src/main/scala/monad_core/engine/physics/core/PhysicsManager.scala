package monad_core.engine.physics.core

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.events.EngineEvent
import monad_core.engine.core.events.EngineEvent.{EntityRemoved, EntityUpdated}
import monad_core.engine.core.traits.{PhysicsEngine, PhysicsStep, State}
import monad_core.engine.geometry.ShapeCollision.shapeCollidesWithShape
import monad_core.engine.geometry.ShapeContainment.shapeContainsPoint
import monad_core.engine.physics.combinators.RuleCombinator
import monad_core.engine.physics.rules.*

/** Physics engine that executes an ordered, configurable collection of rules.
  *
  * @param rules
  *   rules in their execution order
  * @param enabledRules
  *   subset of rules currently enabled
  * @param detector
  *   collision detector used to create each physics context
  */
final case class PhysicsManager private (
    rules: Seq[PhysicsRule],
    enabledRules: Set[PhysicsRule]
)(using detector: CollisionDetector)
    extends PhysicsEngine:

  /** Advances a scene through every enabled physics rule.
    *
    * @param scene
    *   state from which the physics step starts
    * @param deltaTime
    *   elapsed simulation time in nanoseconds
    * @return
    *   updated state and entity events, or the first [[PhysicsError]]
    */
  override def step(scene: State, deltaTime: Long): Either[PhysicsError, PhysicsStep] =
    val activeRules = rules.filter(enabledRules.contains)
    val context     = PhysicsContext.detect(scene, deltaTime)(using detector)

    RuleCombinator.sequence(activeRules)(context).map { result =>
      PhysicsStep(
        state = result.state,
        events = result.events ++ detectEntityStateEvents(scene, result.state)
      )
    }

  /**
    * Returns a manager with the supplied rule enabled.
    *
    * @param rule
    *   rule to enable
    * @return
    *   updated immutable manager
    */
  def enable(rule: PhysicsRule): PhysicsManager =
    copy(enabledRules = enabledRules + rule)

  /**
    * Returns a manager with the supplied rule disabled.
    *
    * @param rule
    *   rule to disable
    * @return
    *   updated immutable manager
    */
  def disable(rule: PhysicsRule): PhysicsManager =
    copy(enabledRules = enabledRules - rule)

  /**
    * Enables every configured rule.
    *
    * @return
    *   manager with all rules enabled
    */
  def enableAll: PhysicsManager =
    copy(enabledRules = rules.toSet)

  /**
    * Disables every configured rule.
    *
    * @return
    *   manager with no enabled rules
    */
  def disableAll: PhysicsManager =
    copy(enabledRules = Set.empty)

  /**
    * Reports whether the supplied rule is enabled.
    *
    * @param rule
    *   rule whose status is requested
    * @return
    *   `true` when the rule is enabled
    */
  def isEnabled(rule: PhysicsRule): Boolean =
    enabledRules.contains(rule)

  /**
    * Detects removals and changes by comparing entity identifiers across two states.
    *
    * @param before
    *   state before rule execution
    * @param after
    *   state after rule execution
    * @return
    *   deterministic sequence of removal and update events
    */
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

  /** Creates a manager with all supplied rules enabled.
    *
    * @param rules
    *   rules in their execution order
    * @param collisionDetector
    *   detector used to build physics contexts
    * @return
    *   configured physics manager
    */
  def apply(
      rules: Vector[PhysicsRule]
  )(using collisionDetector: CollisionDetector): PhysicsManager =
    PhysicsManager(
      rules = rules,
      enabledRules = rules.toSet
    )

  /** Creates a manager containing the standard engine rule pipeline.
    *
    * @return
    *   physics manager with every standard rule enabled
    */
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
