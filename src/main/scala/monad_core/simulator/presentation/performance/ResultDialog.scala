package monad_core.simulator.presentation.performance

import monad_core.simulator.CannotBuildDialog
import monad_core.simulator.errors.BaseError
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, TextArea}
import scalafx.scene.layout.VBox
import scalafx.stage.{Modality, Stage, Window}

import scala.util.Try

/** Mutable handle confined to the graphical boundary of a performance result. */
final class ResultDialogHandle private[performance] (output: TextArea):

  /** Replaces the content displayed by the result window. */
  def update(content: String): Unit =
    output.text = content

/** Small read-only dialog that displays a formatted performance result. */
object ResultDialog:

  /** Default result-window title. */
  val Title = "Performance Test Result"

  /** Preferred width of the result window. */
  private val PreferredWidth = 520.0

  /** Preferred height of the result output. */
  private val PreferredOutputHeight = 360.0

  /** Space between result controls. */
  private val ControlSpacing = 12.0

  /** Padding around the result controls. */
  private val ContentPadding = 20.0

  /**
   * Opens a read-only window containing a formatted report or error message.
   *
   * @param content
   *   text to display
   * @param owner
   *   optional owner window
   * @return
   *   a handle to the shown dialog, or a translated graphical error
   */
  def open(
      content: String,
      owner: Option[Window] = None
  ): Either[BaseError, ResultDialogHandle] =
    Try {
      val selectedOwner = owner
      val stage = new Stage {
        title = Title
        resizable = true
        initModality(Modality.WindowModal)
        selectedOwner.foreach(initOwner)
      }
      val output = new TextArea {
        text = content
        editable = false
        wrapText = false
        prefWidth = PreferredWidth
        prefHeight = PreferredOutputHeight
        styleClass += "performance-result-output"
      }
      val closeButton = new Button("Close") {
        styleClass += "performance-result-close"
        onAction = _ => stage.close()
      }
      val root = new VBox {
        spacing = ControlSpacing
        padding = Insets(ContentPadding)
        alignment = Pos.CenterRight
        children = Seq(output, closeButton)
      }

      stage.scene = new Scene(root)
      stage.sizeToScene()
      stage.show()
      ResultDialogHandle(output)
    }.toEither.left.map(error => CannotBuildDialog(error.getMessage, "PerformanceResultDialog"))

  /**
   * Opens a read-only window containing a formatted report or error message.
   *
   * @param content
   *   text to display
   * @param owner
   *   optional owner window
   * @return
   *   `Right(())` after showing the dialog, or a translated graphical error
   */
  def show(content: String, owner: Option[Window] = None): Either[BaseError, Unit] =
    open(content, owner).map(_ => ())
