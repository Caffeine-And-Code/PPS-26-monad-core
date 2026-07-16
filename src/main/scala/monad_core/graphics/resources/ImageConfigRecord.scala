package monad_core.graphics.resources

trait ImageConfigRecord(val imageBasePath : String)

case class BaseImageConfig() extends ImageConfigRecord(imageBasePath = "/")
