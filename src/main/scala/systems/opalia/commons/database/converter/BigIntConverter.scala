package systems.opalia.commons.database.converter

import scala.util.{Success, Try}
import systems.opalia.interfaces.database.{FieldReader, FieldWriter}


object BigIntConverter
  extends DefaultConverter {

  implicit def readerBigInt: FieldReader[BigInt] =
    new FieldReader[BigInt] {

      def apply(column: String, value: Any): Try[BigInt] =
        readSingle(column, value, (x: BigInt) => x)
    }

  implicit def writerBigInt: FieldWriter[BigInt] =
    new FieldWriter[BigInt] {

      def apply(key: String, value: BigInt): Try[Any] =
        Success(value)
    }
}
