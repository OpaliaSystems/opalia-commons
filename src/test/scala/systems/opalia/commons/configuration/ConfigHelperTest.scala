package systems.opalia.commons.configuration

import com.typesafe.config._
import java.nio.file.{Path, Paths}
import java.time.{OffsetDateTime, OffsetTime}
import org.scalatest._
import scala.concurrent.duration._
import systems.opalia.commons.identifier._
import systems.opalia.commons.net.{EndpointAddress, Uri}


class ConfigHelperTest
  extends FlatSpec
    with Matchers {

  import ConfigHelper._
  import Reader._

  // an example config snippet
  val config = ConfigFactory.parseString(
    """
      |service {
      |  http {
      |    url = "http://localhost:8080"
      |    max_connections = 42
      |    authorization = true
      |    request_timeout = 5 seconds
      |    execution_timeout = Inf
      |    data = /var/www/
      |  }
      |  database {
      |    server = "127.0.0.1:42"
      |    max_memory_usage_in_percent = 93.5
      |    max_db_nodes = 4398046511104
      |    max_stack_size = 512M
      |    initial {
      |      node_1 {
      |        created_at = "1994-11-05T08:15:32Z"
      |        backups_at = "10:15:30Z"
      |        object_id = 9b4dff5a35f1a4e8e2aebca94855b17c6bdda2c7374e5d2d
      |        description = null
      |      }
      |      node_2 {
      |        created_at = "1994-11-05T09:23:03Z"
      |        backups_at = "10:15:30Z"
      |        uuid = 550e8400-e29b-11d4-a716-446655440000
      |        description = "this is a test node"
      |      }
      |    }
      |  }
      |  countries = [DE, US, GB]
      |}
    """.stripMargin)

  it should "signal wrong type conversion" in {

    an[ConfigException.WrongType] should be thrownBy config.as[Option[Long]]("service.http.url")
  }

  it should "signal resolving missing paths" in {

    an[ConfigException.Missing] should be thrownBy config.as[Long]("service.http.undefined")
  }

  it should "handle optional values" in {

    config.as[Option[String]]("service.database.initial.node_1.description") shouldBe None
    config.as[Option[String]]("service.database.initial.node_2.description") shouldBe Some("this is a test node")
    config.as[Option[String]]("service.database.initial.node_3.description") shouldBe None
  }

  it should "handle several collection classes" in {

    val seq = List("DE", "US", "GB")

    val collections =
      (config.as[Seq[String]]("service.countries"),
        config.as[List[String]]("service.countries"),
        config.as[Stream[String]]("service.countries"),
        config.as[Vector[String]]("service.countries"),
        config.as[Array[String]]("service.countries"))

    collections._1.isInstanceOf[Seq[_]] should be(true)
    collections._2.isInstanceOf[List[_]] should be(true)
    collections._3.isInstanceOf[Stream[_]] should be(true)
    collections._4.isInstanceOf[Vector[_]] should be(true)
    collections._5.isInstanceOf[Array[_]] should be(true)

    collections._1 should be(seq)
    collections._2 should be(seq)
    collections._3 should be(seq)
    collections._4 should be(seq)
    collections._5 should be(seq)
  }


  it should "be able to convert native types" in {

    config.as[Boolean]("service.http.authorization") shouldBe true
    config.as[Int]("service.http.max_connections") shouldBe 42
    config.as[Long]("service.database.max_db_nodes") shouldBe 4398046511104l
    config.as[Double]("service.database.max_memory_usage_in_percent") shouldBe 93.5
    config.as[String]("service.http.request_timeout") shouldBe "5 seconds"
  }

  it should "be able to convert additional types" in {

    val httpConfig = config.as[Config]("service.http")
    val databaseConfig = config.as[Config]("service.database")

    databaseConfig.as[ConfigMemorySize]("max_stack_size").toBytes shouldBe 536870912

    httpConfig.as[Duration]("execution_timeout") shouldBe Duration.Inf
    httpConfig.as[FiniteDuration]("request_timeout") shouldBe 5.seconds

    databaseConfig.as[BigInt]("max_db_nodes") shouldBe BigInt(4398046511104l)
    databaseConfig.as[BigDecimal]("max_memory_usage_in_percent") shouldBe BigDecimal(93.5)

    databaseConfig.as[EndpointAddress]("server").toString shouldBe "127.0.0.1:42"

    httpConfig.as[Uri]("url").toString shouldBe "http://localhost:8080"

    httpConfig.as[Path]("data") shouldBe Paths.get("/var/www/")

    databaseConfig.as[OffsetDateTime]("initial.node_1.created_at") shouldBe OffsetDateTime
      .parse("1994-11-05T08:15:32Z")

    databaseConfig.as[OffsetTime]("initial.node_1.backups_at") shouldBe OffsetTime
      .parse("10:15:30Z")

    databaseConfig.as[ObjectId]("initial.node_1.object_id") shouldBe ObjectId
      .getFrom("9b4dff5a35f1a4e8e2aebca94855b17c6bdda2c7374e5d2d")

    databaseConfig.as[UniversallyUniqueId]("initial.node_2.uuid") shouldBe UniversallyUniqueId
      .getFrom("550e8400-e29b-11d4-a716-446655440000")

    an[ConfigException.WrongType] should be thrownBy config.as[Int]("service.database.max_db_nodes")
    an[ConfigException.WrongType] should be thrownBy config.as[FiniteDuration]("service.http.execution_timeout")
  }
}
