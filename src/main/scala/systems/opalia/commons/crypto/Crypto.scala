package systems.opalia.commons.crypto

import java.security._
import javax.crypto._
import javax.crypto.spec.SecretKeySpec
import scala.util.Try


object Crypto {

  object SignAlgorithm
    extends Enumeration {

    val MD5 = Value("MD5")
    val SHA1 = Value("SHA-1")
    val SHA256 = Value("SHA-256")
  }

  object CipherAlgorithm
    extends Enumeration {

    val AES = Value("AES/ECB/PKCS5Padding")
  }

  def sign(data: String, algorithm: SignAlgorithm.Value, secret: Option[String]): Array[Byte] =
    sign(data.getBytes, algorithm, secret)

  def sign(data: Array[Byte], algorithm: SignAlgorithm.Value, secret: Option[String]): Array[Byte] = {

    secret.map {
      secret =>

        // sign with HMAC support

        val algorithmName = "Hmac" + algorithm.toString.filterNot(_ == '-')
        val handler = Mac.getInstance(algorithmName)

        handler.init(new SecretKeySpec(secret.getBytes, algorithmName))
        handler.doFinal(data)

    } getOrElse {

      // sign without HMAC support

      val algorithmName = algorithm.toString
      val handler = MessageDigest.getInstance(algorithmName)

      handler.update(data)
      handler.digest()
    }
  }

  def encrypt(data: String, secret: String, algorithm: CipherAlgorithm.Value): Array[Byte] =
    encrypt(data.getBytes, secret, algorithm)

  def encrypt(data: Array[Byte], secret: String, algorithm: CipherAlgorithm.Value): Array[Byte] = {

    val handler = Cipher.getInstance(algorithm.toString)
    val key = generateKey(secret, algorithm)

    handler.init(Cipher.ENCRYPT_MODE, key)
    handler.doFinal(data)
  }

  def decrypt(data: Array[Byte], secret: String, algorithm: CipherAlgorithm.Value): Array[Byte] = {

    val handler = Cipher.getInstance(algorithm.toString)
    val key = generateKey(secret, algorithm)

    handler.init(Cipher.DECRYPT_MODE, key)
    Try(handler.doFinal(data)).getOrElse(Array())
  }

  private def generateKey(secret: String, algorithm: CipherAlgorithm.Value): SecretKeySpec = {

    val algorithmName = algorithm.toString.takeWhile(_ != '/')
    val length = Cipher.getMaxAllowedKeyLength(algorithmName) / 8
    // max allowed length from bits to bytes
    val signed = sign(secret, SignAlgorithm.SHA256, None)
    val key = new SecretKeySpec(signed.take(length), algorithmName)

    key
  }
}
