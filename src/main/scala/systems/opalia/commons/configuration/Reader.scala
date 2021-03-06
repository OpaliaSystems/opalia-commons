package systems.opalia.commons.configuration

import com.typesafe.config._
import java.nio.charset.Charset
import java.nio.file.{Path, Paths}
import java.time.temporal.TemporalAmount
import java.time.{OffsetDateTime, OffsetTime, Period, Duration => JDuration}
import scala.collection.JavaConverters._
import scala.collection.generic.CanBuildFrom
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.language.higherKinds
import scala.reflect._
import scala.util.{Failure, Success, Try}
import systems.opalia.commons.identifier.{ObjectId, UniversallyUniqueId}
import systems.opalia.commons.net.{EndpointAddress, Uri}
import systems.opalia.commons.time.{SimpleDateTimeParser, SimpleTimeParser}
import systems.opalia.interfaces.logging.LogLevel


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

  implicit def readerJDuration: Reader[JDuration] =
    new Reader[JDuration] {

      def read(config: Config, path: String): Try[JDuration] =
        Try(config.getDuration(path))
    }

  implicit def readerPeriod: Reader[Period] =
    new Reader[Period] {

      def read(config: Config, path: String): Try[Period] =
        Try(config.getPeriod(path))
    }

  implicit def readerTemporalAmount: Reader[TemporalAmount] =
    new Reader[TemporalAmount] {

      def read(config: Config, path: String): Try[TemporalAmount] =
        Try(config.getTemporal(path))
    }

  implicit def readerDuration: Reader[Duration] =
    new Reader[Duration] {

      val typeInfo = s"${classOf[Duration].getName}"

      def read(config: Config, path: String): Try[Duration] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(Duration(value)) match {
                case Failure(e: NumberFormatException) =>
                  Failure(new ConfigException.WrongType(config.origin(), path, typeInfo, classOf[String].getName, e))
                case otherwise =>
                  otherwise
              }
          }
    }

  implicit def readerFiniteDuration: Reader[FiniteDuration] =
    new Reader[FiniteDuration] {

      val typeInfo = s"${classOf[FiniteDuration].getName}"

      def read(config: Config, path: String): Try[FiniteDuration] =
        Try(config.getString(path))
          .flatMap {
            value =>

              (Try(Duration(value)) match {
                case Failure(e: NumberFormatException) =>
                  Failure(new ConfigException.WrongType(config.origin(), path, typeInfo, classOf[String].getName, e))
                case otherwise =>
                  otherwise
              }).flatMap(x => Try(x.asInstanceOf[FiniteDuration]) match {
                case Failure(e) =>
                  Failure(new ConfigException.WrongType(config.origin(), path, typeInfo, classOf[Duration].getName, e))
                case otherwise =>
                  otherwise
              })
          }
    }

  implicit def readerBigInt: Reader[BigInt] =
    new Reader[BigInt] {

      val typeInfo = s"${classOf[BigInt].getName}"

      def read(config: Config, path: String): Try[BigInt] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(BigInt(value)) match {
                case Failure(e: NumberFormatException) =>
                  Failure(new ConfigException.WrongType(config.origin(), path, typeInfo, classOf[String].getName, e))
                case otherwise =>
                  otherwise
              }
          }
    }

  implicit def readerBigDecimal: Reader[BigDecimal] =
    new Reader[BigDecimal] {

      val typeInfo = s"${classOf[BigDecimal].getName}"

      def read(config: Config, path: String): Try[BigDecimal] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(BigDecimal(value)) match {
                case Failure(e: NumberFormatException) =>
                  Failure(new ConfigException.WrongType(config.origin(), path, typeInfo, classOf[String].getName, e))
                case otherwise =>
                  otherwise
              }
          }
    }

  implicit def readerEndpointAddress: Reader[EndpointAddress] =
    new Reader[EndpointAddress] {

      val typeInfo = s"${classOf[EndpointAddress].getName}"

      def read(config: Config, path: String): Try[EndpointAddress] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(EndpointAddress(value)) match {
                case Failure(e) =>
                  Failure(new ConfigException.WrongType(config.origin(), path, typeInfo, classOf[String].getName, e))
                case otherwise =>
                  otherwise
              }
          }
    }

  implicit def readerUri: Reader[Uri] =
    new Reader[Uri] {

      val typeInfo = s"${classOf[Uri].getName}"

      def read(config: Config, path: String): Try[Uri] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(Uri(value)) match {
                case Failure(e) =>
                  Failure(new ConfigException.WrongType(config.origin(), path, typeInfo, classOf[String].getName, e))
                case otherwise =>
                  otherwise
              }
          }
    }

  implicit def readerPath: Reader[Path] =
    new Reader[Path] {

      val typeInfo = s"${classOf[Path].getName}"

      def read(config: Config, path: String): Try[Path] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(Paths.get(value)).filter(x => value == value.trim && value.nonEmpty) match {
                case Failure(e) =>
                  Failure(new ConfigException.WrongType(config.origin(), path, typeInfo, classOf[String].getName, e))
                case otherwise =>
                  otherwise
              }
          }
    }

  implicit def readerCharset: Reader[Charset] =
    new Reader[Charset] {

      val typeInfo = s"${classOf[Charset].getName}"

      def read(config: Config, path: String): Try[Charset] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(Charset.forName(value)) match {
                case Failure(e) =>
                  Failure(new ConfigException.WrongType(config.origin(), path, typeInfo, classOf[String].getName, e))
                case otherwise =>
                  otherwise
              }
          }
    }

  implicit def readerOffsetDateTime: Reader[OffsetDateTime] =
    new Reader[OffsetDateTime] {

      val typeInfo = s"${classOf[OffsetDateTime].getName}"

      def read(config: Config, path: String): Try[OffsetDateTime] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(SimpleDateTimeParser.parse(value)) match {
                case Failure(e) =>
                  Failure(new ConfigException.WrongType(config.origin(), path, typeInfo, classOf[String].getName, e))
                case otherwise =>
                  otherwise
              }
          }
    }

  implicit def readerOffsetTime: Reader[OffsetTime] =
    new Reader[OffsetTime] {

      val typeInfo = s"${classOf[OffsetTime].getName}"

      def read(config: Config, path: String): Try[OffsetTime] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(SimpleTimeParser.parse(value)) match {
                case Failure(e) =>
                  Failure(new ConfigException.WrongType(config.origin(), path, typeInfo, classOf[String].getName, e))
                case otherwise =>
                  otherwise
              }
          }
    }

  implicit def readerObjectId: Reader[ObjectId] =
    new Reader[ObjectId] {

      val typeInfo = s"${classOf[ObjectId].getName}"

      def read(config: Config, path: String): Try[ObjectId] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(ObjectId.getFrom(value)) match {
                case Failure(e) =>
                  Failure(new ConfigException.WrongType(config.origin(), path, typeInfo, classOf[String].getName, e))
                case otherwise =>
                  otherwise
              }
          }
    }

  implicit def readerUniversallyUniqueId: Reader[UniversallyUniqueId] =
    new Reader[UniversallyUniqueId] {

      val typeInfo = s"${classOf[UniversallyUniqueId].getName}"

      def read(config: Config, path: String): Try[UniversallyUniqueId] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(UniversallyUniqueId.getFrom(value)) match {
                case Failure(e) =>
                  Failure(new ConfigException.WrongType(config.origin(), path, typeInfo, classOf[String].getName, e))
                case otherwise =>
                  otherwise
              }
          }
    }

  implicit def readerLogLevel: Reader[LogLevel] =
    new Reader[LogLevel] {

      val typeInfo = s"${classOf[LogLevel].getName} (OFF, ERROR, WARNING, INFO, DEBUG)"

      def read(config: Config, path: String): Try[LogLevel] =
        Try(config.getString(path))
          .flatMap {
            value =>

              Try(LogLevel.withName(value)) match {
                case Failure(e) =>
                  Failure(new ConfigException.WrongType(config.origin(), path, typeInfo, classOf[String].getName, e))
                case otherwise =>
                  otherwise
              }
          }
    }
}
