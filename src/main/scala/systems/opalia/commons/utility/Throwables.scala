package systems.opalia.commons.utility

import java.io.{PrintWriter, StringWriter}


object Throwables {

  def getRootCause(throwable: Throwable): Throwable = {

    getCausalChain(throwable).last
  }

  def getCausalChain(throwable: Throwable): Seq[Throwable] = {

    def traverse(cause: Throwable, acc: List[Throwable]): List[Throwable] = {

      if (cause == null)
        acc
      else if (acc.contains(cause))
        throw new IllegalArgumentException("Cycle in causal chain detected.", cause)
      else
        traverse(cause.getCause, cause +: acc)
    }

    traverse(throwable, Nil)
  }

  def getStackTraceAsString(throwable: Throwable): String = {

    val stringWriter =
      new StringWriter()

    throwable.printStackTrace(new PrintWriter(stringWriter))

    stringWriter.toString
  }
}
