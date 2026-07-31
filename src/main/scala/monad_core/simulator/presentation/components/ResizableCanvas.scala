package monad_core.simulator.presentation.components

import javafx.scene.canvas.Canvas as JfxCanvas
import scalafx.scene.canvas.Canvas

private final class ResizableCanvas extends JfxCanvas:
  override def isResizable: Boolean = true
  override def minWidth(height: Double): Double = 0
  override def minHeight(width: Double): Double = 0
  override def maxWidth(height: Double): Double = Double.MaxValue
  override def maxHeight(width: Double): Double = Double.MaxValue
  override def prefWidth(height: Double): Double = getWidth
  override def prefHeight(width: Double): Double = getHeight
  override def resize(width: Double, height: Double): Unit =
    setWidth(width)
    setHeight(height)

object ResizableCanvas:
  def apply(): Canvas = new Canvas(new ResizableCanvas())