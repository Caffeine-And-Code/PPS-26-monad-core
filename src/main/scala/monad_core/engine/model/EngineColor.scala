package monad_core.engine.model

/** Color representation used by engine drawing commands. */
sealed trait EngineColor

/** Supported color models. */
object EngineColor:

  /**
   * Red-green-blue color representation.
   *
   * @param red red component in the inclusive range `[0, 255]`
   * @param green green component in the inclusive range `[0, 255]`
   * @param blue blue component in the inclusive range `[0, 255]`
   */
  case class RGB(red: RGBValue, green: RGBValue, blue: RGBValue) extends EngineColor

  /**
   * Hue-saturation-lightness color representation.
   *
   * @param hue hue component in the inclusive range `[0, 360]`
   * @param saturation saturation percentage in the inclusive range `[0, 100]`
   * @param lightness lightness percentage in the inclusive range `[0, 100]`
   */
  case class HSL(hue: HueValue, saturation: PercentValue, lightness: PercentValue)
      extends EngineColor

/** Validated RGB component in the inclusive range `[0, 255]`. */
opaque type RGBValue = Int

/** Validated constructor and operations for [[RGBValue]]. */
object RGBValue:

  /**
   * @param rgbValue raw RGB component
   * @return `Right(RGBValue)` when the value is in `[0, 255]`, or `Left(RGBValueCannotExceedRange)` otherwise
   */
  def apply(rgbValue: Int): Either[EngineError, RGBValue] =
    Either.cond(rgbValue >= 0 && rgbValue <= 255, rgbValue, RGBValueCannotExceedRange())

  extension (rgbValue: RGBValue)
    /** Returns the underlying RGB component. */
    def value: Int = rgbValue

/** Validated hue component in the inclusive range `[0, 360]`. */
opaque type HueValue = Int

/** Validated constructor and operations for [[HueValue]]. */
object HueValue:

  /**
   * @param hueValue raw hue component
   * @return `Right(HueValue)` when the value is in `[0, 360]`, or `Left(HueValueCannotExceedRange)` otherwise
   */
  def apply(hueValue: Int): Either[EngineError, HueValue] =
    Either.cond(hueValue >= 0 && hueValue <= 360, hueValue, HueValueCannotExceedRange())

  extension (hueValue: HueValue)
    /** Returns the underlying hue component. */
    def value: Int = hueValue

/** Validated percentage in the inclusive range `[0, 100]`. */
opaque type PercentValue = Int

/** Validated constructor and operations for [[PercentValue]]. */
object PercentValue:

  /**
   * @param percentValue raw percentage
   * @return `Right(PercentValue)` when the value is in `[0, 100]`, or `Left(PercentValueCannotExceedRange)` otherwise
   */
  def apply(percentValue: Int): Either[EngineError, PercentValue] =
    Either.cond(
      percentValue >= 0 && percentValue <= 100,
      percentValue,
      PercentValueCannotExceedRange()
    )

  extension (percentValue: PercentValue)
    /** Returns the underlying percentage. */
    def value: Int = percentValue
