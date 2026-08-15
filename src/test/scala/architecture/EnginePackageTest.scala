package architecture

import architecture.support.ArchitectureChecker
import architecture.support.RetrievablePackage.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EnginePackageTest extends AnyFunSuite with Matchers with ArchitectureChecker:
  val simulatorGuiForbiddenMatcher: String = "monad_core.simulator"

  test("engine should be completely independent from the rest of the application"):
    Engine should notContainImport(simulatorGuiForbiddenMatcher)