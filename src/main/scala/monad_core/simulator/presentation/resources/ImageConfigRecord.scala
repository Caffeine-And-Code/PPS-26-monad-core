package monad_core.simulator.presentation.resources

trait ImageConfigRecord(val imageBasePath : String)

case class BaseImageConfig() extends ImageConfigRecord(imageBasePath = "/images/")
