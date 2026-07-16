package monad_core.graphics.stages.support

import scalafx.beans.binding.NumberBinding
import scalafx.beans.property.ReadOnlyDoubleProperty


case class Size[T](width: T, height: T)

extension (size: Size[ReadOnlyDoubleProperty])

  infix def -(padding: Padding): (NumberBinding, NumberBinding) =
    (
      size.width - padding.horizontalSpacing,
      size.height - padding.verticalSpacing
    )  
