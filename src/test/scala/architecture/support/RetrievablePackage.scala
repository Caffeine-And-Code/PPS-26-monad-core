package architecture.support

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.Using

private object BasePaths:
  private val root: String = "src/main/scala/monad_core"
  val simulatorPackage: String = s"$root/simulator"
  val enginePackage: String = s"$root/engine"

enum RetrievablePackage(val path: Path):
  case Simulator extends RetrievablePackage(Paths.get(BasePaths.simulatorPackage))
  case Engine extends RetrievablePackage(Paths.get(BasePaths.enginePackage))
  case SimulatorApplication extends RetrievablePackage(Paths.get(s"${BasePaths.simulatorPackage}/application"))
  case SimulatorInfrastructure extends RetrievablePackage(Paths.get(s"${BasePaths.simulatorPackage}/infrastructure"))
  case SimulatorPresentation extends RetrievablePackage(Paths.get(s"${BasePaths.simulatorPackage}/presentation"))
  case SimulatorDomain extends RetrievablePackage(Paths.get(s"${BasePaths.simulatorPackage}/domain"))

  def files: List[Path] =
    if !Files.exists(path) || !Files.isDirectory(path)
    then List.empty
    else
      Using.resource(
        Files.walk(path)) {
        _.iterator()
          .asScala
          .filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".scala"))
          .toList
      }