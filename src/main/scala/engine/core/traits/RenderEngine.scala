package engine.core.traits

import engine.core.Scene

trait RenderEngine:
  def render[S](scene: S, alpha: Double): Unit