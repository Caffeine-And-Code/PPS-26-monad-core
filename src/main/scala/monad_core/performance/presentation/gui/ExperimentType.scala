package monad_core.performance.presentation.gui

import monad_core.performance.domain.{PerformanceError, UnknownPerformanceExperimentType}
import monad_core.performance.presentation.{PerformanceArguments, PerformanceRoutes}

/** Graphical selection of a performance command and its experiment-specific arguments. */
enum ExperimentType(
    val label: String,
    val route: String,
    val specificArguments: Vector[String]
):

  /** Sustained expected-load experiment; it needs only the common arguments. */
  case Load extends ExperimentType("Load", PerformanceRoutes.Load, Vector.empty)

  /** Progressive breakpoint search; it also needs maximum entities and growth factor. */
  case Stress
      extends ExperimentType(
        "Stress",
        PerformanceRoutes.Stress,
        Vector(PerformanceArguments.MaximumEntities, PerformanceArguments.GrowthFactor)
      )

  /** Sudden maximum-load experiment; it also needs the maximum entity count. */
  case Spike
      extends ExperimentType(
        "Spike",
        PerformanceRoutes.Spike,
        Vector(PerformanceArguments.MaximumEntities)
      )

  /** Full growth experiment; it also needs maximum entities and growth factor. */
  case Scalability
      extends ExperimentType(
        "Scalability",
        PerformanceRoutes.Scalability,
        Vector(PerformanceArguments.MaximumEntities, PerformanceArguments.GrowthFactor)
      )

object ExperimentType:

  /** Initial selection shown by the graphical form. */
  val Default: ExperimentType = Stress

  /**
   * Resolves a visible selection label.
   *
   * @param label
   *   submitted graphical label
   * @return
   *   matching supported test type, or a validation error
   */
  def fromLabel(label: String): Either[PerformanceError, ExperimentType] =
    values.find(_.label == label).toRight(UnknownPerformanceExperimentType(label))
