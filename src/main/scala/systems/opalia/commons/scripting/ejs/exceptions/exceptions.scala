package systems.opalia.commons.scripting.ejs


package object exceptions {

  abstract class EjsException(message: String)
    extends Exception(message)

  class EjsParsingException(message: String)
    extends EjsException(message)

  class EjsRunningException(message: String)
    extends EjsException(message)

}
