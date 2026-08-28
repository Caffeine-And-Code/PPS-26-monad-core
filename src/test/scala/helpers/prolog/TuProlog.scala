package helpers.prolog

import alice.tuprolog.{Prolog, SolveInfo, Theory}

import scala.util.Using

object TuProlog:

  def solve(theoryResource: String, goal: String): LazyList[SolveInfo] =
    val engine = Prolog()
    engine.setTheory(loadTheory(theoryResource))

    LazyList.unfold(Option(engine.solve(goal))):
      case Some(solution) if solution.isSuccess =>
        val nextSolution =
          Option.when(engine.hasOpenAlternatives)(engine.solveNext())
        Some(solution -> nextSolution)
      case _ => None

  private def loadTheory(theoryResource: String): Theory =
    val stream = Option(getClass.getResourceAsStream(theoryResource)).getOrElse:
      throw IllegalArgumentException(s"Prolog theory not found: $theoryResource")

    Using.resource(stream)(Theory(_))
