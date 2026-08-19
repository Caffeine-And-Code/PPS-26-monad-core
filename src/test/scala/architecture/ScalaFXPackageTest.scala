package architecture

import architecture.support.ArchitectureChecker
import architecture.support.RetrievablePackage.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ScalaFXPackageTest extends AnyFunSuite with Matchers with ArchitectureChecker:
  val scalaFXForbiddenMatcher: String = "scalafx"
  val javaFXForbiddenMatcher: String  = "javafx"

  test("ScalaFX should only be used in Presentation layer of simulator"):
    Engine should notContainImport(scalaFXForbiddenMatcher)
    SimulatorApplication should notContainImport(scalaFXForbiddenMatcher)
    SimulatorDomain should notContainImport(scalaFXForbiddenMatcher)
    SimulatorInfrastructure should notContainImport(scalaFXForbiddenMatcher)

  test("JavaFX should only be used in Presentation layer of simulator"):
    Engine should notContainImport(javaFXForbiddenMatcher)
    SimulatorApplication should notContainImport(javaFXForbiddenMatcher)
    SimulatorDomain should notContainImport(javaFXForbiddenMatcher)
    SimulatorInfrastructure should notContainImport(javaFXForbiddenMatcher)
