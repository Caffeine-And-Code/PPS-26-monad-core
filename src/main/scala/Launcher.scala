import engine.errors.EngineError
import graphics.stages.MainStage

object Launcher {
  def main(args: Array[String]): Unit = {
    MainStage.main(args) match
      case Some(error:EngineError) => println(error.message)
      case None => println("Building Complete.\n")
  }
}