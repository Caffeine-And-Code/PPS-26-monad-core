package monad_core.graphics.panels

import monad_core.graphics.CannotBuildPanel
import monad_core.graphics.helpers.MockImageConfig
import monad_core.graphics.resources.ImageConfigRecord
import monad_core.graphics.support.ScalaFxInit
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.scene.layout.VBox

class GameEngineModePanelTest extends AnyFunSuite with Inside with Matchers with MockFactory with ScalaFxInit:

  test("A GameEngineModePanel can be created"):
    val imageConfigRecord = MockImageConfig()

    val builderResult = GameEngineModePanel.build(imageConfigRecord)

    inside(builderResult):
      case Right(scene) =>
        scene shouldBe a[VBox]

  test("A GameEngineModePanel cannot be built when an invalid image config record is passed"):
    val imageConfigRecord: ImageConfigRecord = mock[ImageConfigRecord]

    val builderResult = GameEngineModePanel.build(imageConfigRecord)

    inside(builderResult):
      case Left(error) =>
        error shouldBe a[CannotBuildPanel]