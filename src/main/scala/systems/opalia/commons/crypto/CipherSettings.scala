package systems.opalia.commons.crypto

import java.security.spec.AlgorithmParameterSpec
import javax.crypto.spec._
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.generators.PKCS5S2ParametersGenerator
import org.bouncycastle.crypto.params.KeyParameter


trait CipherSettings {

  val algorithm: String
  val transformation: String
  val keySaltLength: Int = 64
  val keyLength: Int
  val ivLength: Int
  val supportAEAD: Boolean

  def createAlgorithmParameterSpec(iv: Array[Byte]): AlgorithmParameterSpec = {

    new IvParameterSpec(iv)
  }

  def createSecretKeySpec(secret: String, salt: Array[Byte]): SecretKeySpec = {

    val generator = new PKCS5S2ParametersGenerator(new SHA256Digest())

    generator.init(secret.getBytes("UTF-8"), salt, 65536)

    val bytes = generator.generateDerivedParameters(keyLength * 8).asInstanceOf[KeyParameter].getKey

    new SecretKeySpec(bytes, algorithm)
  }
}

object CipherSettings {

  trait AES
    extends CipherSettings {

    val algorithm: String = "AES"
    val keyLength: Int = 32
  }

  trait TripleDES
    extends CipherSettings {

    val algorithm: String = "DESede"
    val keyLength: Int = 24
  }

  trait Twofish
    extends CipherSettings {

    val algorithm: String = "twofish"
    val keyLength: Int = 32
  }

  object AES_GCM
    extends AES {

    val transformation: String = "AES/GCM/NoPadding"
    val ivLength: Int = 12
    val supportAEAD: Boolean = true

    override def createAlgorithmParameterSpec(iv: Array[Byte]): AlgorithmParameterSpec =
      new GCMParameterSpec(16 * 8, iv)
  }

  object AES_EAX
    extends AES {

    val transformation: String = "AES/EAX/NoPadding"
    val ivLength: Int = 16
    val supportAEAD: Boolean = true
  }

  object AES_CTR
    extends AES {

    val transformation: String = "AES/CTR/PKCS5Padding"
    val ivLength: Int = 16
    val supportAEAD: Boolean = false
  }

  object AES_CBC
    extends AES {

    val transformation: String = "AES/CBC/PKCS5Padding"
    val ivLength: Int = 16
    val supportAEAD: Boolean = false
  }

  object TripleDES_CTR
    extends TripleDES {

    val transformation: String = "DESede/CTR/PKCS5Padding"
    val ivLength: Int = 8
    val supportAEAD: Boolean = false
  }

  object TripleDES_CBC
    extends TripleDES {

    val transformation: String = "DESede/CBC/PKCS5Padding"
    val ivLength: Int = 8
    val supportAEAD: Boolean = false
  }

  object Twofish_CTR
    extends Twofish {

    val transformation: String = "twofish/CTR/PKCS5Padding"
    val ivLength: Int = 16
    val supportAEAD: Boolean = false
  }

  object Twofish_CBC
    extends Twofish {

    val transformation: String = "twofish/CBC/PKCS5Padding"
    val ivLength: Int = 16
    val supportAEAD: Boolean = false
  }

}
