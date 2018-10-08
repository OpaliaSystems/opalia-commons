package systems.opalia.commons.scripting.oql

import java.nio.file.{Path, Paths}
import java.time.{OffsetDateTime, OffsetTime}
import org.parboiled2.ParseError
import org.scalatest._


class ObjectQueryLanguageTest
  extends FlatSpec
    with Matchers {

  trait Model {

    val id: Int

    override def equals(that: Any): Boolean =
      that match {

        case that: Model if (this.getClass == that.getClass && this.id == that.id) => true
        case _ => false
      }

    override def toString: String =
      s"Model(id=$id)"
  }

  it should "be able to resolve paths" in {

    case class Bla(id: Int, foo: Foo)
      extends Model

    case class Foo(id: Int, bar: List[Bar], baz: Baz)
      extends Model

    case class Bar(id: Int, baz: Baz)
      extends Model

    case class Baz(id: Int, var foo: Option[Foo])
      extends Model

    // simple graph to test navigation
    val baz1 = Baz(0, None)
    val baz2 = Baz(1, None)
    val bar1 = Bar(2, baz1)
    val bar2 = Bar(3, baz2)
    val foo1 = Foo(4, List(bar1, bar2), baz1)
    val foo2 = Foo(5, List(bar1, bar2), baz2)
    val bla1 = Bla(6, foo1)
    val bla2 = Bla(7, foo2)

    // cycle
    baz2.foo = Some(foo1)

    val oql =
      new ObjectQueryLanguage(List(
        bla1,
        bla2
      )) {

        def checkRootKey(obj: AnyRef, key: String): Boolean =
          obj match {

            case model: Model if (model.id.toString == key) => true
            case _ => false
          }

        def getObject(obj: AnyRef, segment: Ast.Segment): Option[AnyRef] =
          (obj, segment) match {
            case (m: Bla, Ast.FieldSegment("foo")) => Some(m.foo)
            case (m: Foo, Ast.FieldSegment("bar")) => Some(m.bar)
            case (m: Foo, Ast.KeyAndFieldSegment(k, "bar")) => m.bar.find(_.id.toString == k)
            case (m: Foo, Ast.FieldSegment("baz")) => Some(m.baz)
            case (m: Bar, Ast.FieldSegment("baz")) => Some(m.baz)
            case (m: Baz, Ast.FieldSegment("foo")) => m.foo
            case _ => None
          }

        def getValue(obj: AnyRef, segment: Ast.Segment): Option[Any] =
          None
      }

    oql
      .doFilter()
      .doPagination()
      .doResolution()
      .resolve("~6!(foo)")
      .fetchResult()
      .includes.map(_.asInstanceOf[Model].id) shouldBe List(4)

    oql
      .doFilter()
      .doPagination()
      .doResolution()
      .resolve("~6!(foo.~2!bar)")
      .fetchResult()
      .includes.map(_.asInstanceOf[Model].id) shouldBe List(2)

    oql
      .doFilter()
      .doPagination()
      .doResolution()
      .resolve("~6!(foo.~2!bar.baz)")
      .fetchResult()
      .includes.map(_.asInstanceOf[Model].id) shouldBe List(0)

    oql
      .doFilter()
      .doPagination()
      .doResolution()
      .resolve("~6!(foo.~3!bar.baz.foo)")
      .fetchResult()
      .includes.map(_.asInstanceOf[Model].id) shouldBe List(4)

    oql
      .doFilter()
      .doPagination()
      .doResolution()
      .resolve("~6!(foo.~3!bar.baz.foo.~3!bar.baz.foo)")
      .fetchResult()
      .includes.map(_.asInstanceOf[Model].id) shouldBe List(4)

    oql
      .doFilter()
      .doPagination()
      .doResolution()
      .resolve("~6!(foo.baz)")
      .fetchResult()
      .includes.map(_.asInstanceOf[Model].id) shouldBe List(0)

    oql
      .doFilter()
      .doPagination()
      .doResolution()
      .resolve("foo")
      .fetchResult()
      .includes.map(_.asInstanceOf[Model].id) shouldBe List(4, 5)

    oql
      .doFilter()
      .doPagination()
      .doResolution()
      .resolve("foo.~2!bar")
      .fetchResult()
      .includes.map(_.asInstanceOf[Model].id) shouldBe List(2)

    oql
      .doFilter()
      .doPagination()
      .doResolution()
      .resolve("foo.~2!bar.baz")
      .fetchResult()
      .includes.map(_.asInstanceOf[Model].id) shouldBe List(0)

    oql
      .doFilter()
      .doPagination()
      .doResolution()
      .resolve("foo.baz")
      .fetchResult()
      .includes.map(_.asInstanceOf[Model].id) shouldBe List(0, 1)

    oql
      .doFilter()
      .doPagination()
      .doResolution()
      .resolve("foo.~3!bar.baz;foo.baz")
      .fetchResult()
      .includes.map(_.asInstanceOf[Model].id) shouldBe List(1, 0)

    oql
      .doFilter()
      .doPagination()
      .doResolution()
      .resolve("foo.bar")
      .fetchResult()
      .includes.map(_.asInstanceOf[Model].id) shouldBe List(2, 3)

    // clause with unresolvable paths are ignored
    oql
      .doFilter()
      .doPagination()
      .doResolution()
      .resolve("~6!a.b.c.d")
      .fetchResult()
      .includes.map(_.asInstanceOf[Model].id) shouldBe List()

    // clause with non existing root keys are omitted
    oql
      .doFilter()
      .doPagination()
      .doResolution()
      .resolve("~42!(a.b.c.d)")
      .fetchResult()
      .includes.map(_.asInstanceOf[Model].id) shouldBe List()

    the[ParseError] thrownBy
      oql
        .doFilter()
        .doPagination()
        .doResolution()
        .resolve("foo.~73!bar~baz")

    the[ParseError] thrownBy
      oql
        .doFilter()
        .doPagination()
        .doResolution()
        .resolve("~6")
  }

  it should "handle some types correctly" in {

    case class Values(id: Int,
                      valueBoolean: Boolean,
                      valueByte: Byte,
                      valueShort: Short,
                      valueInt: Int,
                      valueLong: Long,
                      valueFloat: Float,
                      valueDouble: Double,
                      valueBigInt: BigInt,
                      valueBigDecimal: BigDecimal,
                      valueChar: Char,
                      valueString: String,
                      valueOffsetDateTime: OffsetDateTime,
                      valueOffsetTime: OffsetTime,
                      valuePath: Path)
      extends Model

    val oql =
      new ObjectQueryLanguage(List(
        Values(
          id = 0,
          true,
          -20,
          11,
          42,
          73,
          3.14f,
          3.141592653589793d,
          BigInt("100000000000000000000"),
          BigDecimal("100000000000000000000.0815"),
          'a',
          "simple test string",
          OffsetDateTime.parse("2000-01-01T00:30:42.000Z"),
          OffsetTime.parse("00:30:42.000Z"),
          Paths.get("/path/to/resource"))
      )) {

        def checkRootKey(obj: AnyRef, key: String): Boolean =
          obj match {

            case model: Model if (model.id.toString == key) => true
            case _ => false
          }

        def getObject(obj: AnyRef, segment: Ast.Segment): Option[AnyRef] =
          None

        def getValue(obj: AnyRef, segment: Ast.Segment): Option[Any] =
          (obj, segment) match {
            case (m: Values, Ast.FieldSegment("value_boolean")) => Some(m.valueBoolean)
            case (m: Values, Ast.FieldSegment("value_byte")) => Some(m.valueByte)
            case (m: Values, Ast.FieldSegment("value_short")) => Some(m.valueShort)
            case (m: Values, Ast.FieldSegment("value_int")) => Some(m.valueInt)
            case (m: Values, Ast.FieldSegment("value_long")) => Some(m.valueLong)
            case (m: Values, Ast.FieldSegment("value_float")) => Some(m.valueFloat)
            case (m: Values, Ast.FieldSegment("value_double")) => Some(m.valueDouble)
            case (m: Values, Ast.FieldSegment("value_big_int")) => Some(m.valueBigInt)
            case (m: Values, Ast.FieldSegment("value_big_decimal")) => Some(m.valueBigDecimal)
            case (m: Values, Ast.FieldSegment("value_char")) => Some(m.valueChar)
            case (m: Values, Ast.FieldSegment("value_string")) => Some(m.valueString)
            case (m: Values, Ast.FieldSegment("value_offset_date_time")) => Some(m.valueOffsetDateTime)
            case (m: Values, Ast.FieldSegment("value_offset_time")) => Some(m.valueOffsetTime)
            case (m: Values, Ast.FieldSegment("value_path")) => Some(m.valuePath)
            case _ => None
          }
      }

    oql
      .doFilter()
      .filter(
        "and(" +
          "value_boolean?eq(true)," +
          "value_byte?eq(-20)," +
          "value_short?eq(11)," +
          "value_int?eq(42)," +
          "value_long?eq(73)," +
          "value_float?eq(3.14)," +
          "value_double?eq(3.141592653589793)," +
          "value_big_int?eq(100000000000000000000)," +
          "value_big_decimal?eq(100000000000000000000.0815)," +
          "value_char?eq(a)," +
          "value_string?eq(simple test string)," +
          "value_offset_date_time?eq(2000-01-01T00:30:42.000Z)," +
          "value_offset_time?eq(00:30:42.000Z)," +
          "value_path?eq(/path/to/resource)" +
          ")"
      )
      .doPagination()
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(0)

    oql
      .doFilter()
      .filter(
        "and(" +
          "value_boolean?ref.eq(value_boolean)," +
          "value_byte?ref.eq(value_byte)," +
          "value_short?ref.eq(value_short)," +
          "value_int?ref.eq(value_int)," +
          "value_long?ref.eq(value_long)," +
          "value_float?ref.eq(value_float)," +
          "value_double?ref.eq(value_double)," +
          "value_big_int?ref.eq(value_big_int)," +
          "value_big_decimal?ref.eq(value_big_decimal)," +
          "value_char?ref.eq(value_char)," +
          "value_string?ref.eq(value_string)," +
          "value_offset_date_time?ref.eq(value_offset_date_time)," +
          "value_offset_time?ref.eq(value_offset_time)," +
          "value_path?ref.eq(value_path)" +
          ")"
      )
      .doPagination()
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(0)
  }

  it should "handle boolean operations" in {

    case class Values(id: Int, a: Boolean, b: Boolean, c: Boolean)
      extends Model

    val oql =
      new ObjectQueryLanguage(List(
        Values(0, false, false, false),
        Values(1, true, false, false),
        Values(2, false, true, false),
        Values(3, true, true, false),
        Values(4, false, false, true),
        Values(5, true, false, true),
        Values(6, false, true, true),
        Values(7, true, true, true)
      )) {

        def checkRootKey(obj: AnyRef, key: String): Boolean =
          obj match {

            case model: Model if (model.id.toString == key) => true
            case _ => false
          }

        def getObject(obj: AnyRef, segment: Ast.Segment): Option[AnyRef] =
          None

        def getValue(obj: AnyRef, segment: Ast.Segment): Option[Any] =
          (obj, segment) match {
            case (m: Values, Ast.FieldSegment("a")) => Some(m.a)
            case (m: Values, Ast.FieldSegment("b")) => Some(m.b)
            case (m: Values, Ast.FieldSegment("c")) => Some(m.c)
            case _ => None
          }
      }

    oql
      .doFilter()
      .filter("and(a?eq(true),b?eq(true),c?eq(true))")
      .doPagination()
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(7)

    oql
      .doFilter()
      .filter("or(a?eq(true),b?eq(true),c?eq(true))")
      .doPagination()
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(1, 2, 3, 4, 5, 6, 7)

    oql
      .doFilter()
      .filter("xor(a?eq(true),b?eq(true),c?eq(true))")
      .doPagination()
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(1, 2, 4)

    oql
      .doFilter()
      .filter("not(a?eq(true))")
      .doPagination()
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(0, 2, 4, 6)
  }

  it should "allow to apply filter comparison operations" in {

    case class Values(id: Int,
                      valueBoolean: Boolean,
                      valueInt: Int,
                      valueString: String)
      extends Model

    val oql =
      new ObjectQueryLanguage(List(
        Values(0, true, 42, "AbcDefGhi"),
        Values(1, false, 43, "test string ) with ( some special characters '+-_ üäö /"),
        Values(2, true, 72, "abcdefghi"),
        Values(3, false, 73, "ABCDEFGHI")
      )) {

        def checkRootKey(obj: AnyRef, key: String): Boolean =
          obj match {

            case model: Model if (model.id.toString == key) => true
            case _ => false
          }

        def getObject(obj: AnyRef, segment: Ast.Segment): Option[AnyRef] =
          None

        def getValue(obj: AnyRef, segment: Ast.Segment): Option[Any] =
          (obj, segment) match {
            case (m: Values, Ast.FieldSegment("value_boolean")) => Some(m.valueBoolean)
            case (m: Values, Ast.FieldSegment("value_int")) => Some(m.valueInt)
            case (m: Values, Ast.FieldSegment("value_string")) => Some(m.valueString)
            case _ => None
          }
      }

    the[IllegalArgumentException] thrownBy
      oql.doFilter().filter("field?eq(foo)") should have message
      "Cannot resolve left operand field"

    the[IllegalArgumentException] thrownBy
      oql.doFilter().filter("value_int?ref.eq(field)") should have message
      "Cannot resolve right operand field"

    the[IllegalArgumentException] thrownBy
      oql.doFilter().filter("value_boolean?gt(42)") should have message
      "Cannot apply argument on left operand value_boolean with value 42"

    the[IllegalArgumentException] thrownBy
      oql.doFilter().filter("value_int?ref.gt(value_string)") should have message
      "Cannot find matching types for operands value_int and value_string"

    oql
      .doFilter()
      .filter("value_boolean?eq(true);value_int?gt(70)")
      .doPagination()
      .doResolution()
      .fetchResult().initials.map(_.asInstanceOf[Model].id) shouldBe List(2)

    oql
      .doFilter()
      .filter(
        "and(" +
          "value_boolean?eq(true)," +
          "value_boolean?ne(false)," +
          "not(value_boolean?eq(false))" +
          ")"
      )
      .doPagination()
      .doResolution()
      .fetchResult().initials.map(_.asInstanceOf[Model].id) shouldBe List(0, 2)

    oql
      .doFilter()
      .filter(
        "and(" +
          "value_int?gt(70)," +
          "value_int?ge(72)," +
          "value_int?lt(73)," +
          "value_int?le(72)" +
          ")"
      )
      .doPagination()
      .doResolution()
      .fetchResult().initials.map(_.asInstanceOf[Model].id) shouldBe List(2)

    oql
      .doFilter()
      .filter(
        "and(" +
          "value_string?ct(Def)," +
          "value_string?sw(Abc)," +
          "value_string?ew(Ghi)," +
          "value_string?txs.ct(Def)," +
          "value_string?txs.sw(Abc)," +
          "value_string?txs.ew(Ghi)," +
          "value_string?txi.ct(def)," +
          "value_string?txi.sw(abc)," +
          "value_string?txi.ew(ghi)," +
          "value_string?mt([a-zA-Z]+)," +
          "not(value_string?mt([0-9]+))" +
          ")"
      )
      .doPagination()
      .doResolution()
      .fetchResult().initials.map(_.asInstanceOf[Model].id) shouldBe List(0)

    oql
      .doFilter()
      .filter("value_string?eq('''test string ) with ( some special characters '+-_ üäö /''')")
      .doPagination()
      .doResolution()
      .fetchResult().initials.map(_.asInstanceOf[Model].id) shouldBe List(1)
  }

  it should "allow to apply filter comparison operations on sequences" in {

    case class Values(id: Int, a: List[Int], b: Int)
      extends Model

    val oql =
      new ObjectQueryLanguage(List(
        Values(0, List(1, 2, 3, 4), 42)
      )) {

        def checkRootKey(obj: AnyRef, key: String): Boolean =
          obj match {

            case model: Model if (model.id.toString == key) => true
            case _ => false
          }

        def getObject(obj: AnyRef, segment: Ast.Segment): Option[AnyRef] =
          None

        def getValue(obj: AnyRef, segment: Ast.Segment): Option[Any] =
          (obj, segment) match {
            case (m: Values, Ast.FieldSegment("a")) => Some(m.a)
            case (m: Values, Ast.FieldSegment("b")) => Some(m.b)
            case _ => None
          }
      }

    val isTrue =
      List(
        "a?in.eq(3)",
        "a?in.ne(3)",
        "a?in.ne(5)",
        "a?all.ne(5)",
        "a?all.lt(42)",
        "a?ref.all.lt(b)"
      )

    val isFalse =
      List(
        "a?in.eq(5)",
        "a?all.eq(3)",
        "a?all.ne(3)",
        "a?all.eq(5)",
        "a?all.lt(3)"
      )

    isTrue.foreach {
      clause =>

        oql
          .doFilter()
          .filter(clause)
          .doPagination()
          .doResolution()
          .fetchResult()
          .initials should not be empty
    }

    isFalse.foreach {
      clause =>

        oql
          .doFilter()
          .filter(clause)
          .doPagination()
          .doResolution()
          .fetchResult()
          .initials shouldBe empty
    }

    the[IllegalArgumentException] thrownBy
      oql.doFilter().filter("b?in.eq(3)") should have message
      "Cannot apply sequence operation on left operand b"
  }

  it should "be able to sort objects" in {

    case class Values(id: Int,
                      a: Int,
                      b: String,
                      c: String)
      extends Model

    val oql =
      new ObjectQueryLanguage(List(
        Values(0, 10, "Abc", "."),
        Values(1, 11, "Def", ".."),
        Values(2, 12, "abc", "..."),
        Values(3, 13, "def", "...."),
        Values(4, 12, "Abc", "....."),
        Values(5, 13, "Def", "......")
      )) {

        def checkRootKey(obj: AnyRef, key: String): Boolean =
          obj match {

            case model: Model if (model.id.toString == key) => true
            case _ => false
          }

        def getObject(obj: AnyRef, segment: Ast.Segment): Option[AnyRef] =
          None

        def getValue(obj: AnyRef, segment: Ast.Segment): Option[Any] =
          (obj, segment) match {
            case (m: Values, Ast.FieldSegment("a")) => Some(m.a)
            case (m: Values, Ast.FieldSegment("b")) => Some(m.b)
            case (m: Values, Ast.FieldSegment("c")) => Some(m.c)
            case _ => None
          }
      }

    oql
      .doFilter()
      .doPagination()
      .order("(+)a")
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(0, 1, 2, 4, 3, 5)

    oql
      .doFilter()
      .doPagination()
      .order("(-)a")
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(3, 5, 2, 4, 1, 0)

    oql
      .doFilter()
      .doPagination()
      .order("(+)b")
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(0, 4, 1, 5, 2, 3)

    oql
      .doFilter()
      .doPagination()
      .order("(+s)b")
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(0, 4, 1, 5, 2, 3)

    oql
      .doFilter()
      .doPagination()
      .order("(+i)b")
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(0, 2, 4, 1, 3, 5)

    oql
      .doFilter()
      .doPagination()
      .order("(+l)c")
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(0, 1, 2, 3, 4, 5)

    oql
      .doFilter()
      .doPagination()
      .order("(-l)c")
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(5, 4, 3, 2, 1, 0)

    oql
      .doFilter()
      .doPagination()
      .order("(-)a;(+i)b")
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(2, 4, 0, 3, 5, 1)
  }

  it should "be able to truncate the number of resulting objects" in {

    case class Values(id: Int)
      extends Model

    val oql =
      new ObjectQueryLanguage(List(
        Values(0),
        Values(1),
        Values(2),
        Values(3),
        Values(4)
      )) {

        def checkRootKey(obj: AnyRef, key: String): Boolean =
          obj match {

            case model: Model if (model.id.toString == key) => true
            case _ => false
          }

        def getObject(obj: AnyRef, segment: Ast.Segment): Option[AnyRef] =
          None

        def getValue(obj: AnyRef, segment: Ast.Segment): Option[Any] =
          None
      }

    oql
      .doFilter()
      .doPagination()
      .skip("2")
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(2, 3, 4)

    oql
      .doFilter()
      .doPagination()
      .skip("+2")
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(2, 3, 4)

    oql
      .doFilter()
      .doPagination()
      .skip("-2")
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(0, 1, 2)

    oql
      .doFilter()
      .doPagination()
      .limit("2")
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(0, 1)

    oql
      .doFilter()
      .doPagination()
      .limit("-2")
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(3, 4)

    oql
      .doFilter()
      .doPagination()
      .skip("2")
      .limit("1")
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(2)

    oql
      .doFilter()
      .doPagination()
      .skip("-2")
      .limit("-1")
      .doResolution()
      .fetchResult()
      .initials.map(_.asInstanceOf[Values].id) shouldBe List(2)
  }
}
