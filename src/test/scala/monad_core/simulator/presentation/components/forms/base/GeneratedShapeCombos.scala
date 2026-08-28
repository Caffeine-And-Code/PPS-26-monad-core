package monad_core.simulator.presentation.components.forms.base

import alice.tuprolog.{Struct, Term}
import helpers.prolog.TuProlog
import org.scalatest.prop.TableFor3
import org.scalatest.prop.Tables.Table

object GeneratedShapeCombos:

  private val ShapeTheory       = "/prolog/shape_combos.pl"
  private val ShapeGoal         = "shape_field_combo(Radius, Width, Height)."
  private val ExpectedCaseCount = 8

  private val generatedCases: Seq[(Option[String], Option[String], Option[String])] =
    TuProlog
      .solve(ShapeTheory, ShapeGoal)
      .map: solution =>
        (
          optionalValue(solution.getTerm("Radius")),
          optionalValue(solution.getTerm("Width")),
          optionalValue(solution.getTerm("Height"))
        )
      .toList

  require(
    generatedCases.size == ExpectedCaseCount,
    s"Expected $ExpectedCaseCount shape combinations, found ${generatedCases.size}"
  )

  val cases: TableFor3[Option[String], Option[String], Option[String]] = Table(
    ("radius", "width", "height"),
    generatedCases*
  )

  private def optionalValue(term: Term): Option[String] = term match
    case struct: Struct if struct.getName == "none" => None
    case struct: Struct if struct.getName == "some" && struct.getArity == 1 =>
      Some(termValue(struct.getArg(0)))
    case other => throw IllegalArgumentException(s"Not an optional Prolog value: $other")

  private def termValue(term: Term): String = term match
    case atom: Struct => atom.getName
    case other        => other.toString
