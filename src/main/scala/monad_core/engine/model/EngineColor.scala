package monad_core.engine.model

sealed trait EngineColor

object EngineColor:
  case class RGB(red: RGBValue, green: RGBValue, blue: RGBValue) extends EngineColor

  case class HSL(hue: HueValue, saturation: PercentValue, lightness: PercentValue)
      extends EngineColor

opaque type RGBValue = Int

object RGBValue:

  def apply(rgbValue: Int): Either[EngineError, RGBValue] =
    Either.cond(rgbValue >= 0 && rgbValue <= 255, rgbValue, RGBValueCannotExceedRange())

  extension (rgbValue: RGBValue) def value: Int = rgbValue

opaque type HueValue = Int

object HueValue:

  def apply(hueValue: Int): Either[EngineError, HueValue] =
    Either.cond(hueValue >= 0 && hueValue <= 360, hueValue, HueValueCannotExceedRange())

  extension (hueValue: HueValue) def value: Int = hueValue

opaque type PercentValue = Int

object PercentValue:

  def apply(percentValue: Int): Either[EngineError, PercentValue] =
    Either.cond(
      percentValue >= 0 && percentValue <= 100,
      percentValue,
      PercentValueCannotExceedRange()
    )

  extension (percentValue: PercentValue) def value: Int = percentValue
