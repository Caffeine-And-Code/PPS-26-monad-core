package integrations.monad_core.simulator.presentation.components

import integrations.monad_core.simulator.presentation.support.DialogTesting
import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import javafx.event.Event
import javafx.scene.control.CheckMenuItem as JfxCheckMenuItem
import javafx.scene.input.{MouseButton, MouseEvent}
import monad_core.simulator.CannotBuildButton
import monad_core.simulator.presentation.components.MenuButton.toMenuItem
import monad_core.simulator.presentation.components.{
  CheckMenuButtonItem,
  MenuButton,
  MenuButtonItem,
  MenuButtonProps
}
import monad_core.simulator.presentation.resources.{BaseImageConfig, Image}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.beans.property.BooleanProperty
import scalafx.scene.Scene
import scalafx.scene.layout.StackPane
import scalafx.stage.Stage

import scala.jdk.CollectionConverters.*

class MenuButtonTest extends AnyFunSuite with Matchers with Inside with DialogTesting:

  private case class MissingIcon()
      extends Image(fileName = "does-not-exist.png", width = 16, height = 16)

  private def simulateClick(node: scalafx.scene.Node): Unit =
    val event = new MouseEvent(
      MouseEvent.MOUSE_CLICKED,
      0,
      0,
      0,
      0,
      MouseButton.PRIMARY,
      1,
      false,
      false,
      false,
      false,
      true,
      false,
      false,
      true,
      false,
      false,
      null
    )
    Event.fireEvent(node.delegate, event)

  private def showInStage(node: scalafx.scene.Node): Stage =
    val root = new StackPane {
      children = Seq(node)
    }
    val stage = new Stage {
      scene = new Scene(root)
    }
    stage.show()
    stage

  test("toMenuItem should build a MenuItem with the given label"):
    onFxThread {
      val item = MenuButtonItem(label = "My Action", onSelect = () => ())

      item.toMenuItem.text.value should be("My Action")
    }

  test("toMenuItem should invoke onSelect when the item's action fires"):
    onFxThread {
      var selected = false
      val item     = MenuButtonItem(label = "My Action", onSelect = () => selected = true)

      item.toMenuItem.fire()

      selected should be(true)
    }

  test("a checked menu item should report its toggled selection"):
    onFxThread {
      var selected = Option.empty[Boolean]
      val item = CheckMenuButtonItem(
        label = "Rule",
        isSelected = true,
        onToggle = isSelected => selected = Some(isSelected)
      )
      val menuItem = item.toMenuItem.asInstanceOf[JfxCheckMenuItem]
      menuItem.setSelected(false)

      menuItem.fire()

      selected shouldBe Some(false)
    }

  test("a checked menu item should reflect its initial selection"):
    onFxThread {
      val item = CheckMenuButtonItem(
        label = "Rule",
        isSelected = true,
        onToggle = _ => ()
      )

      val menuItem = item.toMenuItem.asInstanceOf[JfxCheckMenuItem]

      menuItem.isSelected shouldBe true
    }

  test("toMenuItem should reflect the initial isDisabled value"):
    onFxThread {
      val disabledItem =
        MenuButtonItem(label = "Disabled", onSelect = () => (), isDisabled = BooleanProperty(true))
      val enabledItem = MenuButtonItem(label = "Enabled", onSelect = () => ())

      disabledItem.toMenuItem.disable.value should be(true)
      enabledItem.toMenuItem.disable.value should be(false)
    }

  test("toMenuItem should stay in sync with isDisabled changes after being built"):
    onFxThread {
      val isDisabled = BooleanProperty(false)
      val item = MenuButtonItem(label = "Toggle", onSelect = () => (), isDisabled = isDisabled)

      val menuItem = item.toMenuItem
      menuItem.disable.value should be(false)

      isDisabled.value = true

      menuItem.disable.value should be(true)
    }

  test("MenuButton.build should return a Node on success"):
    onFxThread {
      val props = MenuButtonProps(
        imageConfig = BaseImageConfig(),
        defaultImage = Image.ToolsIcon(),
        items = Seq(MenuButtonItem(label = "Item 1", onSelect = () => ()))
      )

      val result = MenuButton.build(props)

      inside(result):
        case Right(_) => ()
    }

  test(
    "MenuButton.build should wrap the underlying error in CannotBuildButton when the default image cannot be loaded"
  ):
    onFxThread {
      val props = MenuButtonProps(
        imageConfig = BaseImageConfig(),
        defaultImage = MissingIcon(),
        items = Seq(MenuButtonItem(label = "Item 1", onSelect = () => ()))
      )

      val result = MenuButton.build(props)

      inside(result):
        case Left(error: CannotBuildButton) =>
          error.buttonId should be(MenuButton.toString)
    }

  test(
    "MenuButton.build should wrap the underlying error in CannotBuildButton when the active image cannot be loaded"
  ):
    onFxThread {
      val props = MenuButtonProps(
        imageConfig = BaseImageConfig(),
        defaultImage = Image.ToolsIcon(),
        activeImage = Some(MissingIcon()),
        items = Seq(MenuButtonItem(label = "Item 1", onSelect = () => ()))
      )

      val result = MenuButton.build(props)

      inside(result):
        case Left(error: CannotBuildButton) =>
          error.buttonId should be(MenuButton.toString)
    }

  test("MenuButton should open the context menu when clicked"):
    onFxThread {
      val props = MenuButtonProps(
        imageConfig = BaseImageConfig(),
        defaultImage = Image.ToolsIcon(),
        items = Seq(MenuButtonItem(label = "Item 1", onSelect = () => ()))
      )

      val btn   = MenuButton.build(props).value
      val stage = showInStage(btn)

      simulateClick(btn)

      findOpenContextMenu() should not be empty

      stage.close()
    }

  test("MenuButton should invoke onSelect when a menu item is clicked"):
    onFxThread {
      var selected = false

      val props = MenuButtonProps(
        imageConfig = BaseImageConfig(),
        defaultImage = Image.ToolsIcon(),
        items = Seq(MenuButtonItem(label = "Item 1", onSelect = () => selected = true))
      )

      val btn   = MenuButton.build(props).value
      val stage = showInStage(btn)

      simulateClick(btn)

      val contextMenu = findOpenContextMenu().value
      val menuItem    = contextMenu.getItems.asScala.find(_.getText == "Item 1").value

      menuItem.fire()

      selected should be(true)

      stage.close()
    }

  test(
    "MenuButton should build disabled menu items when the corresponding MenuButtonItem is disabled"
  ):
    onFxThread {
      val props = MenuButtonProps(
        imageConfig = BaseImageConfig(),
        defaultImage = Image.ToolsIcon(),
        items = Seq(
          MenuButtonItem(label = "Enabled", onSelect = () => ()),
          MenuButtonItem(
            label = "Disabled",
            onSelect = () => (),
            isDisabled = BooleanProperty(true)
          )
        )
      )

      val btn   = MenuButton.build(props).value
      val stage = showInStage(btn)

      simulateClick(btn)

      val contextMenu = findOpenContextMenu().value
      contextMenu.getItems.asScala.find(_.getText == "Enabled").value.isDisable should be(false)
      contextMenu.getItems.asScala.find(_.getText == "Disabled").value.isDisable should be(true)

      stage.close()
    }

  test("MenuButton should propagate isDisabled to the underlying button"):
    onFxThread {
      val isDisabled = BooleanProperty(true)
      val props = MenuButtonProps(
        imageConfig = BaseImageConfig(),
        defaultImage = Image.ToolsIcon(),
        items = Seq(MenuButtonItem(label = "Item 1", onSelect = () => ())),
        isDisabled = isDisabled
      )

      val btn = MenuButton.build(props).value

      btn.disable.value should be(true)
    }

  test("MenuButton matches visual snapshot"):
    onFxThread {
      val props = MenuButtonProps(
        imageConfig = BaseImageConfig(),
        defaultImage = Image.ToolsIcon(),
        items = Seq(
          MenuButtonItem(label = "Item 1", onSelect = () => ()),
          MenuButtonItem(label = "Item 2", onSelect = () => ())
        )
      )

      val btn   = MenuButton.build(props).value
      val stage = showInStage(btn)

      assertMatchesVisualSnapshot("menu_button_initial", btn, maxDiffPercentage = 5.0)

      stage.close()
    }

  test("MenuButton matches architectural snapshot"):
    onFxThread {
      val props = MenuButtonProps(
        imageConfig = BaseImageConfig(),
        defaultImage = Image.ToolsIcon(),
        items = Seq(
          MenuButtonItem(label = "Item 1", onSelect = () => ()),
          MenuButtonItem(label = "Item 2", onSelect = () => ())
        )
      )

      val btn   = MenuButton.build(props).value
      val stage = showInStage(btn)

      assertMatchesArchitecturalSnapshotOfStage("menu_button_initial", stage)

      stage.close()
    }
