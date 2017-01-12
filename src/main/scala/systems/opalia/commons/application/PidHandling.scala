package systems.opalia.commons.application

import java.io.{File, FileWriter}
import java.lang.management.ManagementFactory
import systems.opalia.commons.io.FileUtils


trait PidHandling {

  def createPidFile(fileName: String): Unit = {

    val pid = ManagementFactory.getRuntimeMXBean.getName.takeWhile(_ != '@')

    createProcessIdFile()
    sys.addShutdownHook(removeProcessIdFile())

    def createProcessIdFile(): Unit =
      FileUtils.using(new FileWriter(fileName)) {
        fileWriter => fileWriter.write(pid)
      }

    def removeProcessIdFile(): Unit =
      new File(fileName).delete
  }
}
