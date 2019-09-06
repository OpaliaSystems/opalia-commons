package systems.opalia.commons.database.converter

import scala.util.{Success, Try}
import systems.opalia.interfaces.database.{FieldReader, FieldWriter}


object NativeTypesConverter
  extends DefaultConverter {

  implicit def readerBoolean: FieldReader[Boolean] =
    new FieldReader[Boolean] {

      def apply(column: String, value: Any): Try[Boolean] =
        readSingle(column, value, (x: Boolean) => x)
    }

  implicit def writerBoolean: FieldWriter[Boolean] =
    new FieldWriter[Boolean] {

      def apply(key: String, value: Boolean): Try[Any] =
        Success(value)
    }

  implicit def readerByte: FieldReader[Byte] =
    new FieldReader[Byte] {

      def apply(column: String, value: Any): Try[Byte] =
        readSingle(column, value, (x: Byte) => x)
    }

  implicit def writerByte: FieldWriter[Byte] =
    new FieldWriter[Byte] {

      def apply(key: String, value: Byte): Try[Any] =
        Success(value)
    }

  implicit def readerShort: FieldReader[Short] =
    new FieldReader[Short] {

      def apply(column: String, value: Any): Try[Short] =
        readSingle(column, value, (x: Short) => x)
    }

  implicit def writerShort: FieldWriter[Short] =
    new FieldWriter[Short] {

      def apply(key: String, value: Short): Try[Any] =
        Success(value)
    }

  implicit def readerInt: FieldReader[Int] =
    new FieldReader[Int] {

      def apply(column: String, value: Any): Try[Int] =
        readSingle(column, value, (x: Int) => x)
    }

  implicit def writerInt: FieldWriter[Int] =
    new FieldWriter[Int] {

      def apply(key: String, value: Int): Try[Any] =
        Success(value)
    }

  implicit def readerLong: FieldReader[Long] =
    new FieldReader[Long] {

      def apply(column: String, value: Any): Try[Long] =
        readSingle(column, value, (x: Long) => x)
    }

  implicit def writerLong: FieldWriter[Long] =
    new FieldWriter[Long] {

      def apply(key: String, value: Long): Try[Any] =
        Success(value)
    }

  implicit def readerFloat: FieldReader[Float] =
    new FieldReader[Float] {

      def apply(column: String, value: Any): Try[Float] =
        readSingle(column, value, (x: Float) => x)
    }

  implicit def writerFloat: FieldWriter[Float] =
    new FieldWriter[Float] {

      def apply(key: String, value: Float): Try[Any] =
        Success(value)
    }

  implicit def readerDouble: FieldReader[Double] =
    new FieldReader[Double] {

      def apply(column: String, value: Any): Try[Double] =
        readSingle(column, value, (x: Double) => x)
    }

  implicit def writerDouble: FieldWriter[Double] =
    new FieldWriter[Double] {

      def apply(key: String, value: Double): Try[Any] =
        Success(value)
    }

  implicit def readerChar: FieldReader[Char] =
    new FieldReader[Char] {

      def apply(column: String, value: Any): Try[Char] =
        value match {
          case string: String if (string.length == 1) => readSingle(column, string.head, (x: Char) => x)
          case unknown => readSingle(column, unknown, (x: Char) => x)
        }
    }

  implicit def writerChar: FieldWriter[Char] =
    new FieldWriter[Char] {

      def apply(key: String, value: Char): Try[Any] =
        Success(value)
    }

  implicit def readerString: FieldReader[String] =
    new FieldReader[String] {

      def apply(column: String, value: Any): Try[String] =
        readSingle(column, value, (x: String) => x)
    }

  implicit def writerString: FieldWriter[String] =
    new FieldWriter[String] {

      def apply(key: String, value: String): Try[Any] =
        Success(value)
    }
}
