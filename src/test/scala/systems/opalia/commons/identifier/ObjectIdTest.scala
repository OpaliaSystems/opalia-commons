package systems.opalia.commons.identifier

import java.nio.ByteBuffer
import org.scalatest._


class ObjectIdTest
  extends FlatSpec
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

    list.toSet.size should be(list.size)
  }

  it should "have the same machine part" in {

    compare(list, (a: ObjectId, b: ObjectId) => {

      val left =
        ByteBuffer.wrap(Array(
          a(0),
          a(1),
          a(2),
          a(3))).getInt

      val right =
        ByteBuffer.wrap(Array(
          b(0),
          b(1),
          b(2),
          b(3))).getInt

      left == right

    }) should be(true)
  }

  it should "have the same process part" in {

    compare(list, (a: ObjectId, b: ObjectId) => {

      val left =
        ByteBuffer.wrap(Array(
          a(4),
          a(5),
          a(6),
          a(7))).getInt

      val right =
        ByteBuffer.wrap(Array(
          b(4),
          b(5),
          b(6),
          b(7))).getInt

      left == right

    }) should be(true)
  }

  it should "have different counter parts" in {

    compare(list, (a: ObjectId, b: ObjectId) => {

      val left =
        ByteBuffer.wrap(Array(
          a(8),
          a(9),
          a(10),
          a(11))).getInt

      val right =
        ByteBuffer.wrap(Array(
          b(8),
          b(9),
          b(10),
          b(11))).getInt

      left != right

    }) should be(true)
  }

  it should "have similar timestamp parts" in {

    compare(list, (a: ObjectId, b: ObjectId) => {

      val left =
        ByteBuffer.wrap(Array(
          a(12),
          a(13),
          a(14),
          a(15),
          a(16),
          a(17),
          a(18),
          a(19))).getLong

      val right =
        ByteBuffer.wrap(Array(
          b(12),
          b(13),
          b(14),
          b(15),
          b(16),
          b(17),
          b(18),
          b(19))).getLong

      val delta = math.max(left, right) - math.min(left, right)

      delta < 4000

    }) should be(true)
  }

  it should "have different random parts" in {

    compare(list, (a: ObjectId, b: ObjectId) => {

      val left =
        ByteBuffer.wrap(Array(
          a(20),
          a(21),
          a(22),
          a(23))).getInt

      val right =
        ByteBuffer.wrap(Array(
          b(20),
          b(21),
          b(22),
          b(23))).getInt

      left != right

    }) should be(true)
  }
}
