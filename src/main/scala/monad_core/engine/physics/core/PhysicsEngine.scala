package monad_core.engine.physics.core

import monad_core.engine.physics.combinators.RuleCombinator
import monad_core.engine.physics.rules.*

final case class PhysicsEngine[S, CD] private (
                                                rules: Vector[PhysicsRule[S, CD]],
                                                enabledRules: Set[PhysicsRule[S, CD]]
                                              )(using detector: CD) extends Physics[S]:

  override def step(scene: S, deltaTime: Long): Either[PhysicsError, S] =
    val activeRules = rules.filter(enabledRules.contains)
    RuleCombinator.sequence(activeRules)(scene)(using detector, dt = deltaTime)

  def enable(rule: PhysicsRule[S, CD]): PhysicsEngine[S, CD] =
    copy(enabledRules = enabledRules + rule)

  def disable(rule: PhysicsRule[S, CD]): PhysicsEngine[S, CD] =
    copy(enabledRules = enabledRules - rule)

  def enableAll: PhysicsEngine[S, CD] =
    copy(enabledRules = rules.toSet)

  def disableAll: PhysicsEngine[S, CD] =
    copy(enabledRules = Set.empty)

  def isEnabled(rule: PhysicsRule[S, CD]): Boolean =
    enabledRules.contains(rule)

object PhysicsEngine:

  def apply[S, CD](rules: Vector[PhysicsRule[S, CD]])(using detector: CD): PhysicsEngine[S, CD] =
    PhysicsEngine(
      rules = rules,
      enabledRules = rules.toSet
    )

  def default[S, CD](using
                     state: PhysicsState[S],
                     surfaceDetection: SurfaceDetection[CD],
                     collisionDetection: CollisionResolutionDetection[CD],
                     detector: CD
                    ): PhysicsEngine[S, CD] =
    apply(
      Vector(
        EnemyAttractionRule.enemyAttractionRule,
        SurfaceDynamicsRule.surfaceDynamicsRule,
        CollisionResolutionRule.collisionResolutionRule,
        KinematicsRule.kinematicsRule
      )
    )