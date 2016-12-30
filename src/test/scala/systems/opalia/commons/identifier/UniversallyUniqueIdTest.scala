package systems.opalia.commons.identifier

import org.scalatest._


class UniversallyUniqueIdTest
  extends FlatSpec
    with Matchers {

  val list =
    (for (i <- 1 to 1000) yield UniversallyUniqueId.getNew).toList

  it should "be uniqueness" in {

    list.distinct.size should be(list.size)
  }

  it should "have correct variant numbers" in {

    val name = "this is a test name"

    list.foreach {
      randomId =>

        (randomId(6) >> 4) shouldBe 4
    }

    val nameId =
      UniversallyUniqueId.getFromName(name)

    (nameId(6) >> 4) shouldBe 3
  }

  it should "generate same IDs for same names" in {

    val name = "this is a test name"

    UniversallyUniqueId.getFromName(name) shouldBe UniversallyUniqueId.getFromName(name)
  }

  it should "be able to validate strings" in {

    val stringA = "123e4567-e89b-12d3-a456-426655440000"
    val stringB = "123e4567-9b-4212d3-a456-426655440000"
    val stringC = "123e4567-gb42-12d3-a456-426655440000"

    UniversallyUniqueId.isValid(stringA) shouldBe true
    UniversallyUniqueId.isValid(stringB) shouldBe false
    UniversallyUniqueId.isValid(stringC) shouldBe false
  }

  it should "be able to generate from strings" in {

    val stringA = "123e4567-e89b-12d3-a456-426655440000"
    val stringB = "123e4567-9b-4212d3-a456-426655440000"
    val stringC = "123e4567-gb42-12d3-a456-426655440000"

    UniversallyUniqueId.getFromOpt(stringA).map(_.toString) shouldBe Some(stringA)
    UniversallyUniqueId.getFromOpt(stringB) shouldBe None
    UniversallyUniqueId.getFromOpt(stringC) shouldBe None
  }
}
