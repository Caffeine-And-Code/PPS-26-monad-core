package monad_core.graphics.components

import javafx.application.Platform
import javafx.scene.image.ImageView
import monad_core.engine.errors.EngineError
import monad_core.graphics.helpers.{MockImage, MockImageConfig}
import monad_core.graphics.resources.{Image, ImageLoader}
import monad_core.graphics.support.ScalaFxInit
import monad_core.graphics.{CannotBuildButton, ImageResourceNotFound}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.scene.control.Button

import java.util.concurrent.{CountDownLatch, TimeUnit}

class IconButtonTest extends AnyFunSuite with Inside with Matchers with MockFactory with ScalaFxInit:
  private[graphics] case class AlternativeMockImage() extends Image("pause.png", 0, 0)

  val imageConfig = MockImageConfig()
  val primaryImage = MockImage()
  val secondaryImage = AlternativeMockImage()
  val invalidImage: Image = mock[Image]

  private def clickButton(button: Button): Unit =
    val latch = new CountDownLatch(1)

    Platform.runLater: () =>
      button.fire()
      latch.countDown()

    latch.await(5, TimeUnit.SECONDS)

  private def getOrFail[T](either: Either[EngineError, T]): T =
    either match
      case Right(value) => value
      case Left(err) => fail(s"Got error: $err")

  test("A static Icon Button can be built"):
    val props = IconButtonBaseProps(imageConfig)

    val buildResult = IconButton.build(primaryImage, props)

    inside(buildResult):
      case Right(button) =>
        button shouldBe a[Button]

  test("A static Icon Button onClick function is called upon clicking the button"):
    val onClickMock = mockFunction[Boolean, Unit]
    val props = IconButtonBaseProps(imageConfig, onClick = onClickMock)

    val button = getOrFail(IconButton.build(primaryImage, props))

    onClickMock.expects(true).once()

    clickButton(button)

  test("A Static Icon Button can be built with additional applied style"):
    val additionalExpectedStyle = "-fx-background-color: black"
    val props = IconButtonBaseProps(imageConfig, additionalStyle = additionalExpectedStyle)

    val buildResult = IconButton.build(primaryImage, props)

    inside(buildResult):
      case Right(button) =>
        button.style.toString.contains(additionalExpectedStyle) should be(true)

  test("A Static Icon Button can be built and set as disabled"):
    val props = IconButtonBaseProps(imageConfig, isDisabled = true)

    val buildResult = IconButton.build(primaryImage, props)

    inside(buildResult):
      case Right(button) =>
        button.isDisabled should be(true)

  test("If the passed image is invalid the Icon Button is not created and an error is returned"):
    val props = IconButtonBaseProps(imageConfig)

    val buildResult = IconButton.build(invalidImage, props)

    inside(buildResult):
      case Left(error) =>
        error should be(CannotBuildButton(ImageResourceNotFound(invalidImage), IconButton.toString))


  test("A toggle Icon Button can be built"):
    val props = IconButtonBaseProps(imageConfig)

    val buildResult = IconButton.buildToggle(primaryImage, secondaryImage, props)

    inside(buildResult):
      case Right(button) =>
        button shouldBe a[Button]

  test("A toggle Icon Button onClick function is called upon clicking the button"):
    val onClickMock = mockFunction[Boolean, Unit]
    val props = IconButtonBaseProps(imageConfig, onClick = onClickMock)

    val button = getOrFail(IconButton.buildToggle(primaryImage, secondaryImage, props))

    onClickMock.expects(true).once()

    clickButton(button)

  test("A toggle Icon Button can be built with additional applied style"):
    val additionalExpectedStyle = "-fx-background-color: black"
    val props = IconButtonBaseProps(imageConfig, additionalStyle = additionalExpectedStyle)

    val buildResult = IconButton.buildToggle(primaryImage, secondaryImage, props)

    inside(buildResult):
      case Right(button) =>
        button.style.toString.contains(additionalExpectedStyle) should be(true)

  test("A toggle Icon Button can be built and set as disabled"):
    val props = IconButtonBaseProps(imageConfig, isDisabled = true)

    val buildResult = IconButton.buildToggle(primaryImage, secondaryImage, props)

    inside(buildResult):
      case Right(button) =>
        button.isDisabled should be(true)

  test("If the passed image is invalid the toggle Icon Button is not created and an error is returned"):
    val props = IconButtonBaseProps(imageConfig)

    val buildResult = IconButton.buildToggle(invalidImage, invalidImage, props)

    inside(buildResult):
      case Left(error) =>
        error should be(CannotBuildButton(ImageResourceNotFound(invalidImage), IconButton.toString))

  test("Upon clicking a toggle icon button the icon changes"):
    val props = IconButtonBaseProps(imageConfig)
    val expectedImage = getOrFail(ImageLoader.getScalaFxImage(secondaryImage, props.imageConfig))

    val button = getOrFail(IconButton.buildToggle(primaryImage, secondaryImage, props))

    clickButton(button)

    val actualGraphic = button.graphic.value
    actualGraphic shouldBe a[ImageView]

    val actualImage = actualGraphic.asInstanceOf[ImageView].getImage
    actualImage.getUrl should be(expectedImage.delegate.getUrl)

  test("Upon clicking a toggle icon two times the icon changes back to the default image"):
    val props = IconButtonBaseProps(imageConfig)
    val expectedImage = getOrFail(ImageLoader.getScalaFxImage(primaryImage, props.imageConfig))

    val button = getOrFail(IconButton.buildToggle(primaryImage, secondaryImage, props))

    clickButton(button)
    clickButton(button)

    val actualGraphic = button.graphic.value
    actualGraphic shouldBe a[ImageView]

    val actualImage = actualGraphic.asInstanceOf[ImageView].getImage
    actualImage.getUrl should be(expectedImage.delegate.getUrl)