package systems.opalia.commons.identifier

import java.nio.ByteBuffer
import org.scalatest.flatspec._
import org.scalatest.matchers.should._
import systems.opalia.interfaces.rendering.Renderer


class ObjectIdTest
  extends AnyFlatSpec
    with Matchers {

  val list =
    (for (i <- 1 to 1000) yield ObjectId.getNew).toList

  def compare(list: List[ObjectId], f: (ObjectId, ObjectId) => Boolean): Boolean =
    list match {

      case x :: xs =>
        if (xs.forall(y => f(y, x)))
          compare(xs, f)
        else
          false

      case Nil => true
    }

  it should "be uniqueness" in {

    list.distinct.size should be(list.size)
  }

  it should "have the same application part" in {

    compare(list, (a: ObjectId, b: ObjectId) => {

      val left =
        ByteBuffer.wrap(Array(
          a(0),
          a(1),
          a(2),
          a(3)
        )).order(Renderer.appDefaultByteOrder).getInt

      val right =
        ByteBuffer.wrap(Array(
          b(0),
          b(1),
          b(2),
          b(3)
        )).order(Renderer.appDefaultByteOrder).getInt

      left == right

    }) should be(true)
  }

  it should "have different counter parts" in {

    compare(list, (a: ObjectId, b: ObjectId) => {

      val left =
        ByteBuffer.wrap(Array(
          0,
          a(4),
          a(5),
          a(6)
        )).order(Renderer.appDefaultByteOrder).getInt

      val right =
        ByteBuffer.wrap(Array(
          0,
          b(4),
          b(5),
          b(6)
        )).order(Renderer.appDefaultByteOrder).getInt

      left != right

    }) should be(true)
  }

  it should "have similar timestamp parts" in {

    compare(list, (a: ObjectId, b: ObjectId) => {

      val left =
        ByteBuffer.wrap(Array(
          0,
          0,
          0,
          a(7),
          a(8),
          a(9),
          a(10),
          a(11)
        )).order(Renderer.appDefaultByteOrder).getLong

      val right =
        ByteBuffer.wrap(Array(
          0,
          0,
          0,
          b(7),
          b(8),
          b(9),
          b(10),
          b(11)
        )).order(Renderer.appDefaultByteOrder).getLong

      val delta = math.max(left, right) - math.min(left, right)

      delta < 400

    }) should be(true)
  }

  it should "have different random parts" in {

    compare(list, (a: ObjectId, b: ObjectId) => {

      val left =
        ByteBuffer.wrap(Array(
          a(12),
          a(13),
          a(14),
          a(15)
        )).order(Renderer.appDefaultByteOrder).getInt

      val right =
        ByteBuffer.wrap(Array(
          b(12),
          b(13),
          b(14),
          b(15)
        )).order(Renderer.appDefaultByteOrder).getInt

      left != right

    }) should be(true)
  }

  it should "be able to validate strings" in {

    val stringA = "9bdf526da36f09a1b1594d2cf7e6108e"
    val stringB = "9bdf526da36f09a1bx594d2cf7e6108e"

    ObjectId.isValid(stringA) shouldBe true
    ObjectId.isValid(stringB) shouldBe false
  }

  it should "be able to generate from strings" in {

    val stringA = "9bdf526da36f09a1b1594d2cf7e6108e"
    val stringB = "9bdf526da36f09a1bx594d2cf7e6108e"

    ObjectId.getFromOpt(stringA).map(_.toString) shouldBe Some(stringA)
    ObjectId.getFromOpt(stringB) shouldBe None
  }
}
