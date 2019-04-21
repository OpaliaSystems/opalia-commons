package systems.opalia.commons.application

import java.nio.file.{Files, Path}
import systems.opalia.commons.io.FileUtils


abstract class AbstractApplication(pidFilePath: Path)
  extends App {

  FileUtils.using(Files.newBufferedWriter(pidFilePath)) {
    fileWriter => fileWriter.write(SystemProperty.Process.pid.toString)
  }

  pidFilePath.toFile.deleteOnExit()
}
