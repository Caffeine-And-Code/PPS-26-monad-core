package monad_core.simulator.presentation.performance

import monad_core.performance.model.PerformanceError
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.{Error, NotificationManager}
import monad_core.simulator.presentation.components.forms.base.{FormDialog, FormDialogProps}
import scalafx.application.Platform
import scalafx.stage.Window

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** Dynamic parameter form and result orchestration for performance experiments. */
object ExperimentDialog:

  /** Asynchronous performance operation supplied by the application entry point. */
  type RunExperiment = ExperimentCommand => Future[Either[PerformanceError, String]]

  /** Input-window title. */
  val Title = "Performance Test"

  /** Result-window title exposed for presentation integration. */
  val ResultTitle: String = ResultDialog.Title

  private val SubmitLabel    = "Run"
  private val FormWidth      = 520.0
  private val FailureHeader  = "Performance test failed:"
  private val RunningMessage = "Performance test running..."

  /**
   * Opens the performance form and executes its request outside the graphical thread.
   *
   * @param runner asynchronous experiment operation
   * @param owner optional owner of the graphical dialogs
   * @return `Right(())` after opening the form, or its graphical construction error
   */
  def show(
      runner: RunExperiment,
      owner: Option[Window] = None
  ): Either[BaseError, Unit] =
    var resultDialog = Option.empty[ResultDialogHandle]

    def display(content: String): Unit =
      resultDialog = displayResult(content, owner, resultDialog)

    def displayFailure(message: String): Unit =
      display(s"$FailureHeader\n$message")

    def submit(values: Map[String, String]): Unit =
      display(RunningMessage)

      ExperimentForm.command(values) match
        case Left(error) => displayFailure(error.message)
        case Right(command) =>
          runner(command).onComplete {
            case Success(Right(report)) =>
              Platform.runLater(display(report))
            case Success(Left(error)) =>
              Platform.runLater(displayFailure(error.message))
            case Failure(error) =>
              val message = Option(error.getMessage).getOrElse(error.getClass.getSimpleName)
              Platform.runLater(displayFailure(message))
          }(ExecutionContext.parasitic)

    FormDialog.show(
      FormDialogProps(
        title = Title,
        fields = ExperimentForm.fields,
        onSubmit = submit,
        owner = owner,
        minWidth = FormWidth,
        submitLabel = SubmitLabel
      )
    )

  /** Opens or updates the result and reports an unexpected graphical failure. */
  private def displayResult(
      content: String,
      owner: Option[Window],
      currentDialog: Option[ResultDialogHandle]
  ): Option[ResultDialogHandle] =
    currentDialog match
      case Some(dialog) =>
        dialog.update(content)
        currentDialog
      case None =>
        ResultDialog.open(content, owner) match
          case Right(dialog) => Some(dialog)
          case Left(error) =>
            NotificationManager.show(error.message, Error)
            None
