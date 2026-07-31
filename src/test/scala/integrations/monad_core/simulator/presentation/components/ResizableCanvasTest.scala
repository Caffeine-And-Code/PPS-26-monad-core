package integrations.monad_core.simulator.presentation.components

import integrations.monad_core.simulator.presentation.support.{ScalaFxInit, SnapshotTesting}
import monad_core.simulator.presentation.components.ResizableCanvas
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.scene.Scene
import scalafx.scene.layout.StackPane
import scalafx.scene.paint.Color
import scalafx.stage.Stage

class ResizableCanvasTest extends AnyFunSuite with Matchers with ScalaFxInit with SnapshotTesting:

  test("ResizableCanvas should report itself as resizable"):
    runOnFxThread {
      val canvas = ResizableCanvas()

      canvas.delegate.isResizable should be(true)
    }

  test("ResizableCanvas should have no minimum size constraints"):
    runOnFxThread {
      val canvas = ResizableCanvas()

      canvas.delegate.minWidth(-1) should be(0.0)
      canvas.delegate.minHeight(-1) should be(0.0)
    }

  test("ResizableCanvas should have unbounded maximum size constraints"):
    runOnFxThread {
      val canvas = ResizableCanvas()

      canvas.delegate.maxWidth(-1) should be(Double.MaxValue)
      canvas.delegate.maxHeight(-1) should be(Double.MaxValue)
    }

  test("ResizableCanvas should report its preferred size as its current width and height"):
    val expectedWidth = 200
    val expectedHeight = 150

    runOnFxThread {
      val canvas = ResizableCanvas()
      canvas.width = expectedWidth
      canvas.height = expectedHeight

      canvas.delegate.prefWidth(-1) should be(expectedWidth)
      canvas.delegate.prefHeight(-1) should be(expectedHeight)
    }

  test("ResizableCanvas should update its width and height when resized"):
    val expectedWidth = 300
    val expectedHeight = 250

    runOnFxThread {
      val canvas = ResizableCanvas()

      canvas.delegate.resize(expectedWidth, expectedHeight)

      canvas.width.value should be(expectedWidth)
      canvas.height.value should be(expectedHeight)
    }

  test("ResizableCanvas should reflect resize in its own preferred size afterwards"):
    val expectedWidth = 80
    val expectedHeight = 60

    runOnFxThread {
      val canvas = ResizableCanvas()

      canvas.delegate.resize(expectedWidth, expectedHeight)

      canvas.delegate.prefWidth(-1) should be(expectedWidth)
      canvas.delegate.prefHeight(-1) should be(expectedHeight)
    }

  test("ResizableCanvas matches visual snapshot when placed in a resizable container"):
    runOnFxThread {
      val canvas = ResizableCanvas()
      val gc = canvas.graphicsContext2D
      gc.setFill(Color.Red)
      gc.fillRect(0, 0, 100, 100)

      val root = new StackPane {
        prefWidth = 300
        prefHeight = 200
        children = Seq(canvas)
      }

      val stage = new Stage {
        title = "ResizableCanvas Test"
        scene = new Scene(root)
      }
      stage.show()

      assertMatchesVisualSnapshot("resizable_canvas_filled_container", root, maxDiffPercentage = 5.0)
    }

  test("ResizableCanvas matches architectural snapshot when placed in a resizable container"):
    runOnFxThread {
      val canvas = ResizableCanvas()

      val root = new StackPane {
        prefWidth = 300
        prefHeight = 200
        children = Seq(canvas)
      }

      val stage = new Stage {
        title = "ResizableCanvas Test"
        scene = new Scene(root)
      }
      stage.show()

      assertMatchesArchitecturalSnapshotOfStage("resizable_canvas_container", stage)
    }