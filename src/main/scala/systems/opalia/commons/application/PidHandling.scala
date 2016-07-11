package systems.opalia.commons.application

import java.io.{File, FileWriter}
import systems.opalia.commons.io.FileUtils


trait PidHandling {

  def createPidFile(fileName: String): Unit = {

    val pid = SystemProperty.Process.pid

    createProcessIdFile()
    sys.addShutdownHook(removeProcessIdFile())

    def createProcessIdFile(): Unit =
      FileUtils.using(new FileWriter(fileName)) {
        fileWriter => fileWriter.write(pid.toString)
      }

    def removeProcessIdFile(): Unit =
      new File(fileName).delete
  }
}
