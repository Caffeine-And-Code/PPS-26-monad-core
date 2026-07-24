package engine.physics.core

object PhysicsEngine:

  def apply[S, CD](
                    rules: List[PhysicsRule[S, CD]],
                    detector: CD
                  ): Physics[S] =
    new Physics[S]:
      override def step(scene: S, deltaTime: Long): Either[PhysicsError, S] =
        PhysicsEngine.step(rules, detector)(scene, deltaTime)

  def step[S, CD](
                   rules: List[PhysicsRule[S, CD]],
                   detector: CD
                 )(
                   scene: S,
                   deltaTime: Long
                 ): Either[PhysicsError, S] =
    if deltaTime < 0L then
      Left(NegativeDeltaTime(deltaTime))
    else if deltaTime == 0L then
      Right(scene)
    else
      rules.foldLeft[Either[PhysicsError, S]](Right(scene)):
        case (currentScene, rule) =>
          currentScene.flatMap:
            rule.apply(_)(using detector, deltaTime)