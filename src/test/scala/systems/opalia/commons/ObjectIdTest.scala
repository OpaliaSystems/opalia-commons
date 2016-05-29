package systems.opalia.commons

import java.nio.ByteBuffer
import org.scalatest._
import systems.opalia.commons.misc.ObjectId


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

    list.toSet.size should be === list.size
  }

  it should "have the same machine part" in {

    compare(list, (a: ObjectId, b: ObjectId) => {

      val left =
        ByteBuffer.wrap(Array(
          a.toByteArray(0),
          a.toByteArray(1),
          a.toByteArray(2),
          a.toByteArray(3))).getInt

      val right =
        ByteBuffer.wrap(Array(
          b.toByteArray(0),
          b.toByteArray(1),
          b.toByteArray(2),
          b.toByteArray(3))).getInt

      left == right

    }) should be === true
  }

  it should "have the same process part" in {

    compare(list, (a: ObjectId, b: ObjectId) => {

      val left =
        ByteBuffer.wrap(Array(
          a.toByteArray(4),
          a.toByteArray(5),
          a.toByteArray(6),
          a.toByteArray(7))).getInt

      val right =
        ByteBuffer.wrap(Array(
          b.toByteArray(4),
          b.toByteArray(5),
          b.toByteArray(6),
          b.toByteArray(7))).getInt

      left == right

    }) should be === true
  }

  it should "have different counter parts" in {

    compare(list, (a: ObjectId, b: ObjectId) => {

      val left =
        ByteBuffer.wrap(Array(
          a.toByteArray(8),
          a.toByteArray(9),
          a.toByteArray(10),
          a.toByteArray(11))).getInt

      val right =
        ByteBuffer.wrap(Array(
          b.toByteArray(8),
          b.toByteArray(9),
          b.toByteArray(10),
          b.toByteArray(11))).getInt

      left != right

    }) should be === true
  }

  it should "have similar timestamp parts" in {

    compare(list, (a: ObjectId, b: ObjectId) => {

      val left =
        ByteBuffer.wrap(Array(
          a.toByteArray(12),
          a.toByteArray(13),
          a.toByteArray(14),
          a.toByteArray(15),
          a.toByteArray(16),
          a.toByteArray(17),
          a.toByteArray(18),
          a.toByteArray(19))).getLong

      val right =
        ByteBuffer.wrap(Array(
          b.toByteArray(12),
          b.toByteArray(13),
          b.toByteArray(14),
          b.toByteArray(15),
          b.toByteArray(16),
          b.toByteArray(17),
          b.toByteArray(18),
          b.toByteArray(19))).getLong

      val delta = math.max(left, right) - math.min(left, right)

      delta < 4000

    }) should be === true
  }

  it should "have different random parts" in {

    compare(list, (a: ObjectId, b: ObjectId) => {

      val left =
        ByteBuffer.wrap(Array(
          a.toByteArray(20),
          a.toByteArray(21),
          a.toByteArray(22),
          a.toByteArray(23))).getInt

      val right =
        ByteBuffer.wrap(Array(
          b.toByteArray(20),
          b.toByteArray(21),
          b.toByteArray(22),
          b.toByteArray(23))).getInt

      left != right

    }) should be === true
  }
}
