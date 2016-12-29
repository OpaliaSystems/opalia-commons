package systems.opalia.commons.configuration

import com.typesafe.config._
import java.net.URI
import java.nio.file.{Path, Paths}
import scala.collection.JavaConverters._
import scala.collection.generic.CanBuildFrom
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.language.higherKinds
import scala.reflect._
import scala.util.{Failure, Success, Try}
import systems.opalia.commons.identifier.{ObjectId, UniversallyUniqueId}
import systems.opalia.commons.time.DateTime


trait Reader[T] {

  def read(config: Config, path: String): Try[T]
}

object Reader {

  implicit def readerOption[T: ClassTag, U[_]](implicit reader: Reader[T],
                                               u: U[T] => Option[T]): Reader[Option[T]] =
    new Reader[Option[T]] {

      def read(config: Config, path: String): Try[Option[T]] =
        if (config.hasPath(path))
          reader.read(config, path).map(Some(_))
        else
          Success(None)
    }

  implicit def readerSeq[T: ClassTag, U[_]](implicit reader: Reader[T],
                                            u: U[T] => Seq[T],
                                            b: CanBuildFrom[Nothing, T, U[T]]): Reader[U[T]] =
    new Reader[U[T]] {

      def read(config: Config, path: String): Try[U[T]] = {

        Try(config.getList(path).asScala)
          .flatMap {
            value =>

              Try(value.zipWithIndex.map {
                entry =>

                  val entryPath = path + "._" + entry._2
                  val entryConfig = entry._1.atPath(entryPath)

                  reader.read(entryConfig, entryPath).get
              })
          }
          .map(_.to[U])
      }
    }

  implicit def readerArray[T: ClassTag](implicit reader: Reader[T]): Reader[Array[T]] =
    new Reader[Array[T]] {

      def read(config: Config, path: String): Try[Array[T]] = {

        Try(config.getList(path).asScala)
          .flatMap {
            value =>

              Try(value.zipWithIndex.map {
                entry =>

                  val entryPath = path + "._" + entry._2
                  val entryConfig = entry._1.atPath(entryPath)

                  reader.read(entryConfig, entryPath).get
              })
          }
          .map(_.toArray)
      }
    }

  implicit def readerBoolean: Reader[Boolean] =
    new Reader[Boolean] {

      def read(config: Config, path: String): Try[Boolean] =
        Try(config.getBoolean(path))
    }

  implicit def readerInt: Reader[Int] =
    new Reader[Int] {

      def read(config: Config, path: String): Try[Int] =
        Try(config.getInt(path))
    }

  implicit def readerLong: Reader[Long] =
    new Reader[Long] {

      def read(config: Config, path: String): Try[Long] =
        Try(config.getLong(path))
    }

  implicit def readerDouble: Reader[Double] =
    new Reader[Double] {

      def read(config: Config, path: String): Try[Double] =
        Try(config.getDouble(path))
    }

  implicit def readerString: Reader[String] =
    new Reader[String] {

      def read(config: Config, path: String): Try[String] =
        Try(config.getString(path))
    }

  implicit def readerConfig: Reader[Config] =
    new Reader[Config] {

      def read(config: Config, path: String): Try[Config] =
        Try(config.getConfig(path))
    }

  implicit def readerMemorySize: Reader[ConfigMemorySize] =
    new Reader[ConfigMemorySize] {

      def read(config: Config, path: String): Try[ConfigMemorySize] =
        Try(config.getMemorySize(path))
    }

  implicit def readerDuration: Reader[Duration] =
    new Reader[Duration] {

      def read(config: Config, path: String): Try[Duration] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(Duration(value)) match {
                case Failure(e: NumberFormatException) => Failure(new ConfigException.WrongType(
                  config.origin(), path, classOf[Duration].getName, classOf[String].getName, e))
                case otherwise => otherwise
              }
          }
    }

  implicit def readerFiniteDuration: Reader[FiniteDuration] =
    new Reader[FiniteDuration] {

      def read(config: Config, path: String): Try[FiniteDuration] =
        Try(config.getString(path))
          .flatMap {
            value =>

              (Try(Duration(value)) match {
                case Failure(e: NumberFormatException) => Failure(new ConfigException.WrongType(
                  config.origin(), path, classOf[FiniteDuration].getName, classOf[String].getName, e))
                case otherwise => otherwise
              }).flatMap(x => Try(x.asInstanceOf[FiniteDuration]) match {
                case Failure(e) => Failure(new ConfigException.WrongType(
                  config.origin(), path, classOf[FiniteDuration].getName, classOf[String].getName, e))
                case otherwise => otherwise
              })
          }
    }

  implicit def readerBigInt: Reader[BigInt] =
    new Reader[BigInt] {

      def read(config: Config, path: String): Try[BigInt] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(BigInt(value)) match {
                case Failure(e: NumberFormatException) => Failure(new ConfigException.WrongType(
                  config.origin(), path, classOf[BigInt].getName, classOf[String].getName, e))
                case otherwise => otherwise
              }
          }
    }

  implicit def readerBigDecimal: Reader[BigDecimal] =
    new Reader[BigDecimal] {

      def read(config: Config, path: String): Try[BigDecimal] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(BigDecimal(value)) match {
                case Failure(e: NumberFormatException) => Failure(new ConfigException.WrongType(
                  config.origin(), path, classOf[BigDecimal].getName, classOf[String].getName, e))
                case otherwise => otherwise
              }
          }
    }

  implicit def readerURI: Reader[URI] =
    new Reader[URI] {

      def read(config: Config, path: String): Try[URI] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(URI.create(value)) match {
                case Failure(e) => Failure(new ConfigException.WrongType(
                  config.origin(), path, classOf[URI].getName, classOf[String].getName, e))
                case otherwise => otherwise
              }
          }
    }

  implicit def readerPath: Reader[Path] =
    new Reader[Path] {

      def read(config: Config, path: String): Try[Path] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(Paths.get(value)) match {
                case Failure(e) => Failure(new ConfigException.WrongType(
                  config.origin(), path, classOf[Path].getName, classOf[String].getName, e))
                case otherwise => otherwise
              }
          }
    }

  implicit def readerDateTime: Reader[DateTime] =
    new Reader[DateTime] {

      def read(config: Config, path: String): Try[DateTime] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(DateTime.parseIso(value)) match {
                case Failure(e) => Failure(new ConfigException.WrongType(
                  config.origin(), path, classOf[DateTime].getName, classOf[String].getName, e))
                case otherwise => otherwise
              }
          }
    }

  implicit def readerObjectId: Reader[ObjectId] =
    new Reader[ObjectId] {

      def read(config: Config, path: String): Try[ObjectId] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(ObjectId.getFrom(value)) match {
                case Failure(_) => Failure(new ConfigException.WrongType(
                  config.origin(), path, classOf[ObjectId].getName, classOf[String].getName))
                case otherwise => otherwise
              }
          }
    }

  implicit def readerUniversallyUniqueId: Reader[UniversallyUniqueId] =
    new Reader[UniversallyUniqueId] {

      def read(config: Config, path: String): Try[UniversallyUniqueId] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(UniversallyUniqueId.getFrom(value)) match {
                case Failure(_) => Failure(new ConfigException.WrongType(
                  config.origin(), path, classOf[UniversallyUniqueId].getName, classOf[String].getName))
                case otherwise => otherwise
              }
          }
    }
}
