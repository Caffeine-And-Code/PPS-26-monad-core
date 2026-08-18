package monad_core.engine.physics.core

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.{PhysicsEngine, State}
import monad_core.engine.geometry.ShapeCollision.shapeCollidesWithShape
import monad_core.engine.geometry.ShapeContainment.shapeContainsPoint
import monad_core.engine.physics.combinators.RuleCombinator
import monad_core.engine.physics.rules.*

final case class PhysicsManager private (
    rules: Seq[PhysicsRule],
    enabledRules: Set[PhysicsRule]
)(using detector: CollisionDetector)
    extends PhysicsEngine:

  override def step(scene: State, deltaTime: Long): Either[PhysicsError, State] =
    val activeRules  = rules.filter(enabledRules.contains)
    val updatedScene = RuleCombinator.sequence(activeRules)(scene, deltaTime)(using detector)

    updatedScene.left.map(err => err)

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
        CollisionResolutionRule.collisionResolutionRule,
        BorderContactRule.borderContactRule,
        EnemyAttractionRule.enemyAttractionRule,
        KinematicsRule.kinematicsRule
      )
    )
