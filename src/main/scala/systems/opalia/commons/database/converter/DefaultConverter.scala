package systems.opalia.commons.database.converter

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.reflect.{ClassTag, classTag}
import scala.util.{Failure, Success, Try}
import systems.opalia.interfaces.database.{FieldReader, FieldWriter}


abstract class DefaultConverter {

  protected def readSingle[V: ClassTag, U: ClassTag](column: String, value: Any, f: (V) => U): Try[U] =
    value match {
      case null => Failure(new IllegalArgumentException(s"Null value not allowed for column $column."))
      case x: V => Try(f(x))
      case _ => Failure(new IllegalArgumentException(
        s"Cannot convert from $value (${value.asInstanceOf[AnyRef].getClass.getName}) to" +
          s" ${classTag[U].runtimeClass.getName} for column $column."))
    }

  protected def readSequence[V](column: String, value: Any)(implicit reader: FieldReader[V]): Try[Seq[V]] =
    value match {
      case null => Failure(new IllegalArgumentException(s"Null value not allowed for column $column."))
      case x: Seq[_] => Try(x.map(value => reader(column, value).get))
      case _ => Failure(new IllegalArgumentException(
        s"Cannot convert from $value (${value.asInstanceOf[AnyRef].getClass.getName}) to" +
          s" collection class for $column."))
    }
}

object DefaultConverter
  extends DefaultConverter {

  implicit def readerOption[T: ClassTag, U[_]](implicit reader: FieldReader[T],
                                               u: U[T] => Option[T]): FieldReader[U[T]] =
    new FieldReader[U[T]] {

      def apply(column: String, value: Any): Try[U[T]] = {

        val converted =
          if (value != null)
            reader(column, value)
          else
            Failure(new IllegalArgumentException(s"Null value not allowed for column $column."))

        converted match {
          case Success(x) => Success(Option[T](x).asInstanceOf[U[T]])
          case Failure(_) => Success(Option.empty[T].asInstanceOf[U[T]])
        }
      }
    }

  implicit def writerOption[T: ClassTag, U[_]](implicit writer: FieldWriter[T],
                                               u: U[T] => Option[T]): FieldWriter[U[T]] =
    new FieldWriter[U[T]] {

      def apply(key: String, value: U[T]): Try[Any] = {

        if (value != null)
          value.map(writer(key, _)).getOrElse(Success(null))
        else
          Failure(new IllegalArgumentException(s"Null value not allowed for key $key."))
      }
    }

  implicit def writerNone: FieldWriter[None.type] =
    new FieldWriter[None.type] {

      def apply(key: String, value: None.type): Try[Any] =
        Success(null)
    }

  implicit def readerSequence[T: ClassTag, U[_]](implicit reader: FieldReader[T],
                                                 u: U[T] => Seq[T],
                                                 b: CanBuildFrom[Nothing, T, U[T]]): FieldReader[U[T]] =
    new FieldReader[U[T]] {

      def apply(column: String, value: Any): Try[U[T]] =
        readSequence[T](column, value).map(_.to[U])
    }

  implicit def writerSequence[T: ClassTag, U[_]](implicit writer: FieldWriter[T],
                                                 u: U[T] => Seq[T]): FieldWriter[U[T]] =
    new FieldWriter[U[T]] {

      def apply(key: String, value: U[T]): Try[Any] =
        Try(value.toSeq.map(value => writer(key, value).get))
    }

  implicit def readerArray[T: ClassTag](implicit reader: FieldReader[T]): FieldReader[Array[T]] =
    new FieldReader[Array[T]] {

      def apply(column: String, value: Any): Try[Array[T]] =
        readSequence[T](column, value).map(_.toArray)
    }

  implicit def writerArray[T: ClassTag](implicit writer: FieldWriter[T]): FieldWriter[Array[T]] =
    new FieldWriter[Array[T]] {

      def apply(key: String, value: Array[T]): Try[Any] =
        Try(value.toSeq.map(value => writer(key, value).get))
    }
}
