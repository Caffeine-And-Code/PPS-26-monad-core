package architecture.support

import org.scalatest.Suite
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.{MatchResult, Matcher}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

trait ArchitectureChecker extends AnyFunSuite {
  this: Suite =>

  private def extractImports(file: Path): List[String] = {
    Files.readAllLines(file).asScala
      .map(_.trim)
      .filter(_.startsWith("import "))
      .toList
  }

  def checkImports(packageToCheck: RetrievablePackage)(isInvalidImport: String => Boolean): Map[Path, List[String]] = {
    packageToCheck.files.flatMap { file =>
      val invalidImports = extractImports(file).filter(isInvalidImport)
      if (invalidImports.nonEmpty) Some(file -> invalidImports)
      else None
    }.toMap
  }

  def notContainImport(forbiddenSubstring: String): Matcher[RetrievablePackage] = {
    Matcher { packageToCheck =>
      val violations = checkImports(packageToCheck)(imp => imp.contains(forbiddenSubstring))

      val formattedViolations = violations.map { case (file, imports) =>
        val fileRelative = packageToCheck.path.relativize(file)
        val formattedImports = imports.map(i => "      " + i).mkString("\n")
        s"  - $fileRelative:\n$formattedImports"
      }.mkString("\n")

      MatchResult(
        violations.isEmpty,
        s"The package '$packageToCheck' contains forbidden imports with '$forbiddenSubstring':\n$formattedViolations",
        s"The package '$packageToCheck' should not contain '$forbiddenSubstring', but the rule was inverted."
      )
    }
  }
}