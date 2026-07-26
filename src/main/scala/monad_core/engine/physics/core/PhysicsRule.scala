package monad_core.engine.physics.core

trait PhysicsRule[S, CD]:
  val ruleId = ""
  def apply(scene: S)(using detector: CD, dt: Long): Either[PhysicsError, S]

  override def equals(obj: Any): Boolean = obj match
    case that: PhysicsRule[?, ?] if this.ruleId.nonEmpty && that.ruleId.nonEmpty =>
      this.ruleId == that.ruleId
    case _ => super.equals(obj)

  override def hashCode(): Int =
    if ruleId.nonEmpty then ruleId.hashCode else super.hashCode()