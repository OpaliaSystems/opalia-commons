package systems.opalia.commons.database.converter

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.util.{Success, Try}
import systems.opalia.interfaces.database.{FieldReader, FieldWriter}


object TimeTypesConverter
  extends DefaultConverter {

  implicit def readerLocalDate: FieldReader[LocalDate] =
    new FieldReader[LocalDate] {

      def apply(column: String, value: Any): Try[LocalDate] =
        readSingle(column, value, (x: LocalDate) => x)
    }

  implicit def writerLocalDate: FieldWriter[LocalDate] =
    new FieldWriter[LocalDate] {

      def apply(key: String, value: LocalDate): Try[Any] =
        Success(value)
    }

  implicit def readerLocalTime: FieldReader[LocalTime] =
    new FieldReader[LocalTime] {

      def apply(column: String, value: Any): Try[LocalTime] =
        readSingle(column, value, (x: LocalTime) => x)
    }

  implicit def writerLocalTime: FieldWriter[LocalTime] =
    new FieldWriter[LocalTime] {

      def apply(key: String, value: LocalTime): Try[Any] =
        Success(value)
    }

  implicit def readerLocalDateTime: FieldReader[LocalDateTime] =
    new FieldReader[LocalDateTime] {

      def apply(column: String, value: Any): Try[LocalDateTime] =
        readSingle(column, value, (x: LocalDateTime) => x)
    }

  implicit def writerLocalDateTime: FieldWriter[LocalDateTime] =
    new FieldWriter[LocalDateTime] {

      def apply(key: String, value: LocalDateTime): Try[Any] =
        Success(value)
    }
}
