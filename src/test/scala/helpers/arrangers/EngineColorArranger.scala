package helpers.arrangers

import monad_core.engine.model.EngineColor.RGB
import monad_core.engine.model.{EngineColor, RGBValue}
import org.scalatest.EitherValues.convertEitherToValuable

object EngineColorArranger:
  
  private def arrangeColor(red:Int, green:Int, blue: Int) : EngineColor =
    RGB(RGBValue(red).value, RGBValue(green).value, RGBValue(blue).value)

  def arrangeRed(): EngineColor =
    arrangeColor(255,0,0)
  
  def arrangeWhite():EngineColor =
    arrangeColor(255,255,255)
    
  def arrangeBlack():EngineColor =
    arrangeColor(0,0,0)