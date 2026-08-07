package architecture

import architecture.support.ArchitectureChecker
import architecture.support.RetrievablePackage.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SimulatorPackageTest extends AnyFunSuite with Matchers with ArchitectureChecker:
  val engineForbiddenMatcher: String = "monad_core.engine"
  val presentationForbiddenMatcher: String = "monad_core.simulator.presentation"
  val infrastructureForbiddenMatcher: String = "monad_core.simulator.infrastructure"
  val applicationForbiddenMatcher: String = "monad_core.simulator.application"

  test("engine package in simulator should only be used in infrastructure layer"):
    val packageExceptions: Set[String] = Set("public_api")

    SimulatorPresentation should notContainImportWithExceptions(engineForbiddenMatcher, packageExceptions)
    SimulatorDomain should notContainImportWithExceptions(engineForbiddenMatcher, packageExceptions)
    SimulatorApplication should notContainImportWithExceptions(engineForbiddenMatcher, packageExceptions)

  test("presentation layer should not be utilized in any other layer of the simulator package"):
    SimulatorApplication should notContainImport(presentationForbiddenMatcher)
    SimulatorInfrastructure should notContainImport(presentationForbiddenMatcher)
    SimulatorDomain should notContainImport(presentationForbiddenMatcher)
    Engine should notContainImport(presentationForbiddenMatcher)

  test("application layer should only be used in presentation and infrastructure layer or in itself"):
    Engine should notContainImport(applicationForbiddenMatcher)
    SimulatorDomain should notContainImport(applicationForbiddenMatcher)

  test("infrastructure layer should only be used in application layer"):
    SimulatorPresentation should notContainImport(infrastructureForbiddenMatcher)
    Engine should notContainImport(infrastructureForbiddenMatcher)
    SimulatorDomain should notContainImport(infrastructureForbiddenMatcher)