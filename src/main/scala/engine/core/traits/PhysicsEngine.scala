package engine.core.traits

trait PhysicsEngine :
  def step(updaterEngine: UpdaterEngine, dt: Long): UpdaterEngine
