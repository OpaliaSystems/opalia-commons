package systems.opalia.commons.application


object SystemProperty {

  val defaultCharset: String = "UTF-8"

  object OperatingSystem {

    def name: String =
      Option(System.getProperty("os.name"))
        .getOrElse(throw new IllegalStateException("Cannot get name property of OS."))

    def version: String =
      Option(System.getProperty("os.version"))
        .getOrElse(throw new IllegalStateException("Cannot get version property of OS."))

    def architecture: String =
      Option(System.getProperty("os.arch"))
        .getOrElse(throw new IllegalStateException("Cannot get architecture property of OS."))
  }

  object Runtime {

    def name: String =
      Option(System.getProperty("java.vm.name"))
        .getOrElse(throw new IllegalStateException("Cannot get name property of VM."))

    def version: String =
      Option(System.getProperty("java.vm.version"))
        .getOrElse(throw new IllegalStateException("Cannot get version property of VM."))

    def vendor: String =
      Option(System.getProperty("java.vm.vendor"))
        .getOrElse(throw new IllegalStateException("Cannot get vendor property of VM."))
  }

  object Process {

    def pid: Int = {

      try {

        val bean =
          Class.forName("java.lang.management.ManagementFactory")
            .getMethod("getRuntimeMXBean").invoke(null)

        val method =
          bean.getClass.getDeclaredMethod("getName")

        method.setAccessible(true)

        val result = method.invoke(bean).asInstanceOf[String]
        val pid = result.takeWhile(_ != '@').toInt

        pid

      } catch {

        case _: Throwable => {

          try {

            val pid =
              Class.forName("android.os.Process")
                .getMethod("myPid").invoke(null).asInstanceOf[Int]

            pid

          } catch {

            case _: Throwable =>
              throw new IllegalStateException("Cannot get PID of current process.")
          }
        }
      }
    }
  }

}
