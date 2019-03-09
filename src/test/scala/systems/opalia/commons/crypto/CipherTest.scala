package systems.opalia.commons.crypto

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import javax.crypto.{AEADBadTagException, BadPaddingException}
import org.scalatest._
import scala.io.Source
import systems.opalia.interfaces.rendering.Renderer


class CipherTest
  extends FlatSpec
    with Matchers {

  val configs =
    List(
      CipherSettings.AES_GCM, CipherSettings.AES_EAX, CipherSettings.AES_CTR, CipherSettings.AES_CBC,
      CipherSettings.TripleDES_CTR, CipherSettings.TripleDES_CBC,
      CipherSettings.Twofish_CTR, CipherSettings.Twofish_CBC
    )

  it should "encrypt/decrypt AES with correct result" in {

    configs.foreach {
      config =>

        val message = "this is a secret message"

        val cipher1 = Cipher(config, "password 1")
        val cipher2 = Cipher(config, "password 2")

        val encrypted = cipher1.encrypt(message)

        val anyPosSalt = 5
        val anyPosIV = cipher1.settings.keySaltLength + 5
        val anyPosCipherText = cipher1.settings.keySaltLength + cipher1.settings.ivLength + 5
        val anyPosTag = encrypted.length - 5

        val corrupted1 = encrypted.updated(anyPosSalt, (encrypted(anyPosSalt) + 1).toByte)
        val corrupted2 = encrypted.updated(anyPosIV, (encrypted(anyPosIV) + 1).toByte)
        val corrupted3 = encrypted.updated(anyPosCipherText, (encrypted(anyPosCipherText) + 1).toByte)
        val corrupted4 = encrypted.updated(anyPosTag, (encrypted(anyPosTag) + 1).toByte)

        if (cipher1.settings.supportAEAD) {

          // incorrect secret key
          an[AEADBadTagException] should be thrownBy cipher2.decrypt(encrypted)

          // corrupted AAD
          an[AEADBadTagException] should be thrownBy cipher1.decrypt(corrupted1)
          an[AEADBadTagException] should be thrownBy cipher1.decrypt(corrupted2)

          // corrupted cipher text
          an[AEADBadTagException] should be thrownBy cipher1.decrypt(corrupted3)

          // corrupted tag
          an[AEADBadTagException] should be thrownBy cipher1.decrypt(corrupted4)

        } else {

          // incorrect secret key
          an[BadPaddingException] should be thrownBy cipher2.decrypt(encrypted)

          // corrupted key salt data
          an[BadPaddingException] should be thrownBy cipher1.decrypt(corrupted1)

          // corrupted IV data
          an[BadPaddingException] should be thrownBy cipher1.decrypt(corrupted1)
        }

        val decrypted = cipher1.decrypt(encrypted)

        new String(decrypted.toArray, Renderer.appDefaultCharset) should be(message)
    }
  }

  it should "encrypt/decrypt AES using InputStream with correct result" in {

    configs.foreach {
      config =>

        val message = "this is a secret message"

        val cipher = Cipher(config, "password")

        val messageIn = new ByteArrayInputStream(message.getBytes(Renderer.appDefaultCharset))
        val encryptIn = cipher.encrypt(messageIn)
        val decryptIn = cipher.decrypt(encryptIn)

        val messageOut = new ByteArrayOutputStream()
        val decryptOut = cipher.decrypt(messageOut)
        val encryptOut = cipher.encrypt(decryptOut)

        encryptOut.write(message.getBytes(Renderer.appDefaultCharset))
        encryptOut.flush()
        encryptOut.close()

        Source.fromInputStream(decryptIn, Renderer.appDefaultCharset.name).mkString should be(message)
        new String(messageOut.toByteArray, Renderer.appDefaultCharset) should be(message)
    }
  }
}
