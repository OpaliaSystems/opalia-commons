package systems.opalia.commons.number


object Imports {

  implicit class StringToNumberImprovements(string: String) {

    import scala.util.control.Exception._

    def toByteOpt: Option[Byte] =
      catching(classOf[NumberFormatException]) opt string.toByte

    def toShortOpt: Option[Short] =
      catching(classOf[NumberFormatException]) opt string.toShort

    def toIntOpt: Option[Int] =
      catching(classOf[NumberFormatException]) opt string.toInt

    def toLongOpt: Option[Long] =
      catching(classOf[NumberFormatException]) opt string.toLong

    def toFloatOpt: Option[Float] =
      catching(classOf[NumberFormatException]) opt string.toFloat

    def toDoubleOpt: Option[Double] =
      catching(classOf[NumberFormatException]) opt string.toDouble

    def toBigInt: BigInt =
      BigInt(string)

    def toBigIntOpt: Option[BigInt] =
      catching(classOf[NumberFormatException]) opt BigInt(string)

    def toBigDecimal: BigDecimal =
      BigDecimal(string)

    def toBigDecimalOpt: Option[BigDecimal] =
      catching(classOf[NumberFormatException]) opt BigDecimal(string)

    def toStrictBoolean: Boolean =
      if (string.toLowerCase == "true" || string.toLowerCase == "on" || string.toLowerCase == "yes")
        true
      else if (string.toLowerCase == "false" || string.toLowerCase == "off" || string.toLowerCase == "no")
        false
      else
        throw new NumberFormatException(s"Cannot get boolean from string: $string")

    def toStrictBooleanOpt: Option[Boolean] =
      catching(classOf[NumberFormatException]) opt string.toStrictBoolean
  }

}
