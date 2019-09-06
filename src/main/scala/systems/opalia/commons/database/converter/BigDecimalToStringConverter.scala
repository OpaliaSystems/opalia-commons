package systems.opalia.commons.database.converter

import scala.util.{Success, Try}
import systems.opalia.interfaces.database.{FieldReader, FieldWriter}


object BigDecimalToStringConverter
  extends DefaultConverter {

  implicit def readerBigDecimal: FieldReader[BigDecimal] =
    new FieldReader[BigDecimal] {

      def apply(column: String, value: Any): Try[BigDecimal] =
        readSingle(column, value, (x: String) => BigDecimal(x))
    }

  implicit def writerBigDecimal: FieldWriter[BigDecimal] =
    new FieldWriter[BigDecimal] {

      def apply(key: String, value: BigDecimal): Try[Any] =
        Success(value.toString)
    }
}
