package monad_core.engine.model

import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EngineColorTest extends AnyFunSuite with Matchers with Inside:

  test("EngineColor.RGB can be constructed from three valid RGBValues"):
    val red   = RGBValue(255).value
    val green = RGBValue(0).value
    val blue  = RGBValue(128).value

    val color = EngineColor.RGB(red, green, blue)

    color.red should be(red)
    color.green should be(green)
    color.blue should be(blue)

  test("EngineColor.HSL can be constructed from valid Hue and Percent values"):
    val hue        = HueValue(200).value
    val saturation = PercentValue(80).value
    val lightness  = PercentValue(60).value

    val color = EngineColor.HSL(hue, saturation, lightness)

    color.hue should be(hue)
    color.saturation should be(saturation)
    color.lightness should be(lightness)

  test("Two EngineColor.RGB built from equal components are equal"):
    val first  = EngineColor.RGB(RGBValue(10).value, RGBValue(20).value, RGBValue(30).value)
    val second = EngineColor.RGB(RGBValue(10).value, RGBValue(20).value, RGBValue(30).value)

    first should be(second)

  test("Two EngineColor.HSL built from equal components are equal"):
    val first  = EngineColor.HSL(HueValue(10).value, PercentValue(20).value, PercentValue(30).value)
    val second = EngineColor.HSL(HueValue(10).value, PercentValue(20).value, PercentValue(30).value)

    first should be(second)
