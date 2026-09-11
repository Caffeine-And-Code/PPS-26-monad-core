package monad_core.simulator.presentation.components

import javafx.scene.canvas.Canvas as JfxCanvas
import scalafx.scene.canvas.Canvas

/** JavaFX canvas implementation that participates in parent layout resizing. */
final private class ResizableCanvas extends JfxCanvas:
  override def isResizable: Boolean              = true
  override def minWidth(height: Double): Double  = 0
  override def minHeight(width: Double): Double  = 0
  override def maxWidth(height: Double): Double  = Double.MaxValue
  override def maxHeight(width: Double): Double  = Double.MaxValue
  override def prefWidth(height: Double): Double = getWidth
  override def prefHeight(width: Double): Double = getHeight

  override def resize(width: Double, height: Double): Unit =
    setWidth(width)
    setHeight(height)

/** Factory for resizable ScalaFX canvases. */
object ResizableCanvas:

  /**
   * Creates a canvas whose dimensions can be managed by a resizable layout container.
   *
   * @return
   *   a ScalaFX canvas with zero minimum size, unbounded maximum size and preferred size equal to its current size
   */
  def apply(): Canvas = new Canvas(new ResizableCanvas())
