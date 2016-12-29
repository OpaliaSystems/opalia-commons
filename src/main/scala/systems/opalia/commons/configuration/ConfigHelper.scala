package systems.opalia.commons.configuration

import com.typesafe.config.Config


object ConfigHelper {

  implicit class ConfigImprovements(config: Config) {

    def as[T](path: String)(implicit reader: Reader[T]): T =
      reader.read(config, path: String).get
  }

}
