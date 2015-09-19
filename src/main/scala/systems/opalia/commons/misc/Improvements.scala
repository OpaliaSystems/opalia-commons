package systems.opalia.commons.misc

import scala.language.implicitConversions


object Improvements {

  implicit class StringImprovements(x: String) {

    import scala.util.control.Exception._

    def toByteOpt: Option[Byte] =
      catching(classOf[NumberFormatException]) opt x.toByte

    def toShortOpt: Option[Short] =
      catching(classOf[NumberFormatException]) opt x.toShort

    def toIntOpt: Option[Int] =
      catching(classOf[NumberFormatException]) opt x.toInt

    def toLongOpt: Option[Long] =
      catching(classOf[NumberFormatException]) opt x.toLong

    def toFloatOpt: Option[Float] =
      catching(classOf[NumberFormatException]) opt x.toFloat

    def toDoubleOpt: Option[Double] =
      catching(classOf[NumberFormatException]) opt x.toDouble

    def toBigInt: BigInt =
      BigInt(x)

    def toBigIntOpt: Option[BigInt] =
      catching(classOf[NumberFormatException]) opt BigInt(x)

    def toBigDecimal: BigDecimal =
      BigDecimal(x)

    def toBigDecimalOpt: Option[BigDecimal] =
      catching(classOf[NumberFormatException]) opt BigDecimal(x)

    def toBooleanOption: Option[Boolean] =
      if (x.toLowerCase == "true" || x.toLowerCase == "on" || x.toLowerCase == "yes" || x == "1")
        Some(true)
      else if (x.toLowerCase == "false" || x.toLowerCase == "off" || x.toLowerCase == "no" || x == "0")
        Some(false)
      else
        None
  }

  implicit class ByteImprovements(x: Byte) {

    def big: BigDecimal =
      BigDecimal.decimal(x)
  }

  implicit class ShortImprovements(x: Short) {

    def big: BigDecimal =
      BigDecimal.decimal(x)
  }

  implicit class IntImprovements(x: Int) {

    def big: BigDecimal =
      BigDecimal.decimal(x)
  }

  implicit class LongImprovements(x: Long) {

    def big: BigDecimal =
      BigDecimal.decimal(x)
  }

  implicit class FloatImprovements(x: Float) {

    def big: BigDecimal =
      BigDecimal.decimal(x)
  }

  implicit class DoubleImprovements(x: Double) {

    def big: BigDecimal =
      BigDecimal.decimal(x)
  }

}
