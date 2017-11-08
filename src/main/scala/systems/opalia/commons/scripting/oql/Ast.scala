package systems.opalia.commons.scripting.oql

import java.time.{OffsetDateTime, OffsetTime}
import java.util
import scala.collection.JavaConverters._
import scala.util.matching.Regex
import scala.util.{Success, Try}
import systems.opalia.commons.number.Imports._
import systems.opalia.commons.time.{SimpleDateTimeParser, SimpleTimeParser}


object Ast {

  private[oql] object TextMode
    extends Enumeration {

    val None, Sensitive, Insensitive, Length = Value
  }

  private[oql] object SequenceMode
    extends Enumeration {

    val None, AtAll, AtLeastOne = Value
  }

  private[oql] case class FilterProperty(term: BooleanTerm, rootKey: Option[String])

  private[oql] case class OrderProperty(path: Path, ascending: Boolean, textMode: TextMode.Value)

  private[oql] case class ResolveProperty(path: Path, rootKey: Option[String])

  private[oql] case class Path(init: List[Segment], last: Segment) {

    override def toString: String =
      (init :+ last).map(_.toString).mkString(".")
  }

  private[oql] case class Value(value: String)

  sealed trait Segment

  case class KeyAndFieldSegment(key: String, field: String)
    extends Segment {

    override def toString: String =
      s"~$key!$field"
  }

  case class FieldSegment(field: String)
    extends Segment {

    override def toString: String =
      field
  }

  private[oql] sealed trait BooleanTerm {

    def evaluate(resolve: (Path) => Option[Any]): Try[Boolean]
  }

  private[oql] sealed trait BooleanFunction
    extends BooleanTerm {

    val values: List[BooleanTerm]
  }

  private[oql] case class And(values: List[BooleanTerm])
    extends BooleanFunction {

    def evaluate(resolve: (Path) => Option[Any]): Try[Boolean] =
      Try(!values.exists(_.evaluate(resolve).get == false))
  }

  private[oql] case class Or(values: List[BooleanTerm])
    extends BooleanFunction {

    def evaluate(resolve: (Path) => Option[Any]): Try[Boolean] =
      Try(values.exists(_.evaluate(resolve).get == true))
  }

  private[oql] case class Xor(values: List[BooleanTerm])
    extends BooleanFunction {

    def evaluate(resolve: (Path) => Option[Any]): Try[Boolean] =
      Try(values.foldLeft(0)((a, b) => if (a > 1) a else a + (if (b.evaluate(resolve).get) 1 else 0)) == 1)
  }

  private[oql] case class Not(value: BooleanTerm)
    extends BooleanFunction {

    val values: List[Ast.BooleanTerm] = List(value)

    def evaluate(resolve: (Path) => Option[Any]): Try[Boolean] =
      value.evaluate(resolve).map(x => !x)
  }

  private[oql] case class Exists(path: Path)
    extends BooleanTerm {

    def evaluate(resolve: (Path) => Option[Any]): Try[Boolean] =
      resolve(path) match {
        case Some(_) => Success(true)
        case None => Success(false)

      }
  }

  private[oql] sealed trait BooleanComparator
    extends BooleanTerm {

    val path: Path
    val value: Either[Value, Path]
    val sequenceMode: SequenceMode.Value
    val textMode: TextMode.Value

    protected val matchString: PartialFunction[(Any, String, TextMode.Value), Boolean]
    protected val matchAny: PartialFunction[(Any, Any, TextMode.Value), Boolean]

    def evaluate(resolve: (Path) => Option[Any]): Try[Boolean] = {

      def apply(): Boolean =
        resolve(path) match {
          case None =>
            throw new IllegalArgumentException(s"Cannot resolve left operand $path")
          case Some(seq: util.Collection[_]) if (sequenceMode == SequenceMode.AtAll) =>
            seq.asScala.forall(applyRight)
          case Some(seq: Seq[_]) if (sequenceMode == SequenceMode.AtAll) =>
            seq.forall(applyRight)
          case Some(seq: Array[_]) if (sequenceMode == SequenceMode.AtAll) =>
            seq.forall(applyRight)
          case Some(seq: util.Collection[_]) if (sequenceMode == SequenceMode.AtLeastOne) =>
            seq.asScala.exists(applyRight)
          case Some(seq: Seq[_]) if (sequenceMode == SequenceMode.AtLeastOne) =>
            seq.exists(applyRight)
          case Some(seq: Array[_]) if (sequenceMode == SequenceMode.AtLeastOne) =>
            seq.exists(applyRight)
          case Some(x) if (sequenceMode == SequenceMode.None) =>
            applyRight(x)
          case _ =>
            throw new IllegalArgumentException(s"Cannot apply sequence operation on left operand $path")
        }

      def applyRight(x: Any): Boolean =
        value match {
          case Left(Value(y)) =>
            matchString.applyOrElse((x, y, textMode),
              (_: (Any, String, TextMode.Value)) =>
                throw new IllegalArgumentException(s"Cannot apply argument on left operand $path with value $y"))
          case Right(y) =>
            resolve(y) match {
              case None =>
                throw new IllegalArgumentException(s"Cannot resolve right operand $y")
              case Some(z) =>
                matchAny.applyOrElse((x, z, textMode),
                  (_: (Any, Any, TextMode.Value)) =>
                    throw new IllegalArgumentException(s"Cannot find matching types for operands $path and $y"))
            }
        }

      Try(apply())
    }
  }

  private[oql] sealed trait BooleanComparatorEqual
    extends BooleanComparator {

    val matchString: PartialFunction[(Any, String, TextMode.Value), Boolean] = {
      case (left: Boolean, right, TextMode.None) =>
        equalsOp(Try(left == right.toStrictBoolean).getOrElse(false))
      case (left: Byte, right, TextMode.None) =>
        equalsOp(Try(BigDecimal.decimal(left) == BigDecimal(right)).getOrElse(false))
      case (left: Short, right, TextMode.None) =>
        equalsOp(Try(BigDecimal.decimal(left) == BigDecimal(right)).getOrElse(false))
      case (left: Int, right, TextMode.None) =>
        equalsOp(Try(BigDecimal.decimal(left) == BigDecimal(right)).getOrElse(false))
      case (left: Long, right, TextMode.None) =>
        equalsOp(Try(BigDecimal.decimal(left) == BigDecimal(right)).getOrElse(false))
      case (left: Float, right, TextMode.None) =>
        equalsOp(Try(BigDecimal.decimal(left) == BigDecimal(right)).getOrElse(false))
      case (left: Double, right, TextMode.None) =>
        equalsOp(Try(BigDecimal.decimal(left) == BigDecimal(right)).getOrElse(false))
      case (left: Char, right, TextMode.None) =>
        equalsOp(if (right.length == 1) left == right.head else false)
      case (left: BigInt, right, TextMode.None) =>
        equalsOp(Try(BigDecimal(left) == BigDecimal(right)).getOrElse(false))
      case (left: BigDecimal, right, TextMode.None) =>
        equalsOp(Try(left == BigDecimal(right)).getOrElse(false))
      case (left: String, right, TextMode.None) =>
        equalsOp(left.equals(right))
      case (left: String, right, TextMode.Sensitive) =>
        equalsOp(left.equals(right))
      case (left: String, right, TextMode.Insensitive) =>
        equalsOp(left.equalsIgnoreCase(right))
      case (left: String, right, TextMode.Length) =>
        equalsOp(left.length == right.length)
      case (left: OffsetDateTime, right, TextMode.None) =>
        equalsOp(Try(left == SimpleDateTimeParser.parse(right)).getOrElse(false))
      case (left: OffsetTime, right, TextMode.None) =>
        equalsOp(Try(left == SimpleTimeParser.parse(right)).getOrElse(false))
      case (left, right, TextMode.None) =>
        equalsOp(left.toString == right)
    }

    val matchAny: PartialFunction[(Any, Any, TextMode.Value), Boolean] = {
      case (left: String, right: String, TextMode.None) =>
        equalsOp(left.equals(right))
      case (left: String, right: String, TextMode.Sensitive) =>
        equalsOp(left.equals(right))
      case (left: String, right: String, TextMode.Insensitive) =>
        equalsOp(left.equalsIgnoreCase(right))
      case (left: String, right: String, TextMode.Length) =>
        equalsOp(left.length == right.length)
      case (left, right, TextMode.None) =>
        equalsOp(left == right)
    }

    def equalsOp(value: Boolean): Boolean
  }

  private[oql] sealed trait BooleanComparatorOrdered
    extends BooleanComparator {

    val matchString: PartialFunction[(Any, String, TextMode.Value), Boolean] = {
      case (left: Byte, right, TextMode.None) =>
        Try(compareOp(BigDecimal.decimal(left).compareTo(BigDecimal(right)))).getOrElse(false)
      case (left: Short, right, TextMode.None) =>
        Try(compareOp(BigDecimal.decimal(left).compareTo(BigDecimal(right)))).getOrElse(false)
      case (left: Int, right, TextMode.None) =>
        Try(compareOp(BigDecimal.decimal(left).compareTo(BigDecimal(right)))).getOrElse(false)
      case (left: Long, right, TextMode.None) =>
        Try(compareOp(BigDecimal.decimal(left).compareTo(BigDecimal(right)))).getOrElse(false)
      case (left: Float, right, TextMode.None) =>
        Try(compareOp(BigDecimal.decimal(left).compareTo(BigDecimal(right)))).getOrElse(false)
      case (left: Double, right, TextMode.None) =>
        Try(compareOp(BigDecimal.decimal(left).compareTo(BigDecimal(right)))).getOrElse(false)
      case (left: Char, right, TextMode.None) =>
        if (right.length == 1) compareOp(left.compareTo(right.head)) else false
      case (left: BigInt, right, TextMode.None) =>
        Try(compareOp(BigDecimal(left).compareTo(BigDecimal(right)))).getOrElse(false)
      case (left: BigDecimal, right, TextMode.None) =>
        Try(compareOp(left.compareTo(BigDecimal(right)))).getOrElse(false)
      case (left: String, right, TextMode.None) =>
        compareOp(left.compareTo(right))
      case (left: String, right, TextMode.Sensitive) =>
        compareOp(left.compareTo(right))
      case (left: String, right, TextMode.Insensitive) =>
        compareOp(left.toLowerCase.compareTo(right.toLowerCase))
      case (left: String, right, TextMode.Length) =>
        compareOp(left.length.compareTo(right.length))
      case (left: OffsetDateTime, right, TextMode.None) =>
        Try(compareOp(left.compareTo(SimpleDateTimeParser.parse(right)))).getOrElse(false)
      case (left: OffsetTime, right, TextMode.None) =>
        Try(compareOp(left.compareTo(SimpleTimeParser.parse(right)))).getOrElse(false)
    }

    val matchAny: PartialFunction[(Any, Any, TextMode.Value), Boolean] = {
      case (left: Byte, right: Byte, TextMode.None) =>
        compareOp(left.compareTo(right))
      case (left: Short, right: Short, TextMode.None) =>
        compareOp(left.compareTo(right))
      case (left: Int, right: Int, TextMode.None) =>
        compareOp(left.compareTo(right))
      case (left: Long, right: Long, TextMode.None) =>
        compareOp(left.compareTo(right))
      case (left: Float, right: Float, TextMode.None) =>
        compareOp(left.compareTo(right))
      case (left: Double, right: Double, TextMode.None) =>
        compareOp(left.compareTo(right))
      case (left: Char, right: Char, TextMode.None) =>
        compareOp(left.compareTo(right))
      case (left: BigInt, right: BigInt, TextMode.None) =>
        compareOp(left.compareTo(right))
      case (left: BigDecimal, right: BigDecimal, TextMode.None) =>
        compareOp(left.compareTo(right))
      case (left: String, right: String, TextMode.None) =>
        compareOp(left.compareTo(right))
      case (left: String, right: String, TextMode.Sensitive) =>
        compareOp(left.compareTo(right))
      case (left: String, right: String, TextMode.Insensitive) =>
        compareOp(left.toLowerCase.compareTo(right.toLowerCase))
      case (left: String, right: String, TextMode.Length) =>
        compareOp(left.length.compareTo(right.length))
      case (left: OffsetDateTime, right: OffsetDateTime, TextMode.None) =>
        compareOp(left.compareTo(right))
      case (left: OffsetTime, right: OffsetTime, TextMode.None) =>
        compareOp(left.compareTo(right))
    }

    def compareOp(value: Int): Boolean
  }

  private[oql] case class Equal(path: Path,
                                value: Either[Value, Path],
                                sequenceMode: SequenceMode.Value,
                                textMode: TextMode.Value)
    extends BooleanComparatorEqual {

    def equalsOp(value: Boolean): Boolean =
      value
  }

  private[oql] case class NotEqual(path: Path,
                                   value: Either[Value, Path],
                                   sequenceMode: SequenceMode.Value,
                                   textMode: TextMode.Value)
    extends BooleanComparatorEqual {

    def equalsOp(value: Boolean): Boolean =
      !value
  }

  private[oql] case class GreaterThan(path: Path,
                                      value: Either[Value, Path],
                                      sequenceMode: SequenceMode.Value,
                                      textMode: TextMode.Value)
    extends BooleanComparatorOrdered {

    def compareOp(value: Int): Boolean =
      value > 0
  }

  private[oql] case class GreaterThanOrEqual(path: Path,
                                             value: Either[Value, Path],
                                             sequenceMode: SequenceMode.Value,
                                             textMode: TextMode.Value)
    extends BooleanComparatorOrdered {

    def compareOp(value: Int): Boolean =
      value >= 0
  }

  private[oql] case class LessThan(path: Path,
                                   value: Either[Value, Path],
                                   sequenceMode: SequenceMode.Value,
                                   textMode: TextMode.Value)
    extends BooleanComparatorOrdered {

    def compareOp(value: Int): Boolean =
      value < 0
  }

  private[oql] case class LessThanOrEqual(path: Path,
                                          value: Either[Value, Path],
                                          sequenceMode: SequenceMode.Value,
                                          textMode: TextMode.Value)
    extends BooleanComparatorOrdered {

    def compareOp(value: Int): Boolean =
      value <= 0
  }

  private[oql] case class Contains(path: Path,
                                   value: Either[Value, Path],
                                   sequenceMode: SequenceMode.Value,
                                   textMode: TextMode.Value)
    extends BooleanComparator {

    val matchString: PartialFunction[(Any, String, TextMode.Value), Boolean] = {
      case (left: String, right, TextMode.None | TextMode.Sensitive) =>
        left.contains(right)
      case (left: String, right, TextMode.Insensitive) =>
        left.toLowerCase.contains(right.toLowerCase)
    }

    val matchAny: PartialFunction[(Any, Any, TextMode.Value), Boolean] = {
      case (x: String, y: String, z@(TextMode.Sensitive | TextMode.Insensitive)) =>
        matchString(x, y, z)
    }
  }

  private[oql] case class StartsWith(path: Path,
                                     value: Either[Value, Path],
                                     sequenceMode: SequenceMode.Value,
                                     textMode: TextMode.Value)
    extends BooleanComparator {

    val matchString: PartialFunction[(Any, String, TextMode.Value), Boolean] = {
      case (left: String, right, TextMode.None | TextMode.Sensitive) =>
        left.startsWith(right)
      case (left: String, right, TextMode.Insensitive) =>
        left.toLowerCase.startsWith(right.toLowerCase)
    }

    val matchAny: PartialFunction[(Any, Any, TextMode.Value), Boolean] = {
      case (x: String, y: String, z@(TextMode.Sensitive | TextMode.Insensitive)) =>
        matchString(x, y, z)
    }
  }

  private[oql] case class EndsWith(path: Path,
                                   value: Either[Value, Path],
                                   sequenceMode: SequenceMode.Value,
                                   textMode: TextMode.Value)
    extends BooleanComparator {

    val matchString: PartialFunction[(Any, String, TextMode.Value), Boolean] = {
      case (left: String, right, TextMode.None | TextMode.Sensitive) =>
        left.endsWith(right)
      case (left: String, right, TextMode.Insensitive) =>
        left.toLowerCase.endsWith(right.toLowerCase)
    }

    val matchAny: PartialFunction[(Any, Any, TextMode.Value), Boolean] = {
      case (x: String, y: String, z@(TextMode.Sensitive | TextMode.Insensitive)) =>
        matchString(x, y, z)
    }
  }

  private[oql] case class Matches(path: Path,
                                  value: Either[Value, Path],
                                  sequenceMode: SequenceMode.Value,
                                  textMode: TextMode.Value)
    extends BooleanComparator {

    val matchString: PartialFunction[(Any, String, TextMode.Value), Boolean] = {
      case (left: String, right, TextMode.None) =>
        Try(right.r)
          .map(_.pattern.matcher(left).matches)
          .recover {

            case e: Throwable =>
              throw new IllegalArgumentException(
                s"Cannot generate regular expression from string: $right\n${e.getMessage}", e)
          }
          .get
    }

    val matchAny: PartialFunction[(Any, Any, TextMode.Value), Boolean] = {
      case (x: String, y: String, TextMode.None) =>
        matchString(x, y, TextMode.None)
      case (left: String, right: Regex, TextMode.None) =>
        right.pattern.matcher(left).matches
    }
  }

}
