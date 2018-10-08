package systems.opalia.commons.scripting.oql

import java.util
import org.parboiled2._
import scala.collection.JavaConverters._
import scala.collection.mutable


abstract class ObjectQueryLanguage(objects: List[AnyRef]) {

  def doFilter(): ObjectQueryLanguage.Filter =
    new ObjectQueryLanguage.Filter(this, objects)

  protected def checkRootKey(obj: AnyRef, key: String): Boolean

  protected def getObject(obj: AnyRef, segment: Ast.Segment): Option[AnyRef]

  protected def getValue(obj: AnyRef, segment: Ast.Segment): Option[Any]
}

object ObjectQueryLanguage {

  private class ObjectQueryLanguageParser(val input: ParserInput)
    extends Parser {

    private val CharSeq1 = CharPredicate.AlphaNum ++ "-_"
    private val CharSeq2 = CharPredicate.AlphaNum ++ "_"

    def `filter-expression`: Rule1[List[Ast.FilterProperty]] =
      rule {

        (oneOrMore(`filter-property`).separatedBy(';') ~> (_.toList)) ~ EOI
      }

    def `resolve-expression`: Rule1[List[Ast.ResolveProperty]] =
      rule {

        (oneOrMore(`resolve-property`).separatedBy(';') ~> (_.toList)) ~ EOI
      }

    def `order-expression`: Rule1[List[Ast.OrderProperty]] =
      rule {

        (oneOrMore(`order-property`).separatedBy(';') ~> (_.toList)) ~ EOI
      }

    def `skip-expression`: Rule1[Int] =
      rule {

        `value-integer` ~ EOI
      }

    def `limit-expression`: Rule1[Int] =
      rule {

        `value-integer` ~ EOI
      }

    def `filter-property`: Rule1[Ast.FilterProperty] =
      rule {

        `filter-root-path` | `boolean-term` ~> ((x: Ast.BooleanTerm) => Ast.FilterProperty(x, None))

      }

    def `resolve-property`: Rule1[Ast.ResolveProperty] =
      rule {

        `resolve-root-path` | `path` ~> ((x: Ast.Path) => Ast.ResolveProperty(x, None))
      }

    def `order-property`: Rule1[Ast.OrderProperty] =
      rule {

        ('(' ~ (
          '+' ~ push(true) |
            '-' ~ push(false)
          ) ~ (
          's' ~ push(Ast.TextMode.Sensitive) |
            'i' ~ push(Ast.TextMode.Insensitive) |
            'l' ~ push(Ast.TextMode.Length) |
            push(Ast.TextMode.None)
          ) ~ ')' ~ `path`) ~> {
          (ascending: Boolean, textMode: Ast.TextMode.Value, path: Ast.Path) =>

            Ast.OrderProperty(path, ascending, textMode)
        }
      }

    def `filter-root-path`: Rule1[Ast.FilterProperty] =
      rule {

        "~" ~ `value-key` ~ ("!(" ~ `boolean-term` ~ ")") ~> {
          (key: String, term: Ast.BooleanTerm) =>

            Ast.FilterProperty(term, Some(key))
        }
      }

    def `resolve-root-path`: Rule1[Ast.ResolveProperty] =
      rule {

        "~" ~ `value-key` ~ ("!(" ~ `path` ~ ")") ~> {
          (key: String, path: Ast.Path) =>

            Ast.ResolveProperty(path, Some(key))
        }
      }

    def `path`: Rule1[Ast.Path] =
      rule {

        oneOrMore(`segment`).separatedBy('.') ~> {
          (x: Seq[Ast.Segment]) =>

            Ast.Path(x.init.toList, x.last)
        }
      }

    def `segment`: Rule1[Ast.Segment] =
      rule {

        `key-and-field-segment` | `field-segment`
      }

    def `key-and-field-segment`: Rule1[Ast.KeyAndFieldSegment] =
      rule {

        "~" ~ `value-key` ~ "!" ~ `value-field` ~> {
          (key: String, field: String) =>

            Ast.KeyAndFieldSegment(key, field)
        }
      }

    def `field-segment`: Rule1[Ast.FieldSegment] =
      rule {

        `value-field` ~> {
          (x: String) =>

            Ast.FieldSegment(x)
        }
      }

    def `boolean-term`: Rule1[Ast.BooleanTerm] =
      rule {

        `boolean-function` | `boolean-exists` | `boolean-comparator`
      }

    def `boolean-function`: Rule1[Ast.BooleanFunction] =
      rule {

        `boolean-function-AND` | `boolean-function-OR` | `boolean-function-XOR` | `boolean-function-NOT`
      }

    def `boolean-comparator`: Rule1[Ast.BooleanComparator] =
      rule {

        `boolean-cmp-eq` |
          `boolean-cmp-ne` |
          `boolean-cmp-gt` |
          `boolean-cmp-ge` |
          `boolean-cmp-lt` |
          `boolean-cmp-le` |
          `boolean-cmp-ct` |
          `boolean-cmp-sw` |
          `boolean-cmp-ew` |
          `boolean-cmp-mt`
      }

    def `boolean-function-AND`: Rule1[Ast.And] =
      rule {

        "and(" ~ oneOrMore(`boolean-term`).separatedBy(',') ~ ')' ~> {
          (x: Seq[Ast.BooleanTerm]) =>

            Ast.And(x.toList)
        }
      }

    def `boolean-function-OR`: Rule1[Ast.Or] =
      rule {

        "or(" ~ oneOrMore(`boolean-term`).separatedBy(',') ~ ')' ~> {
          (x: Seq[Ast.BooleanTerm]) =>

            Ast.Or(x.toList)
        }
      }

    def `boolean-function-XOR`: Rule1[Ast.Xor] =
      rule {

        "xor(" ~ oneOrMore(`boolean-term`).separatedBy(',') ~ ')' ~> {
          (x: Seq[Ast.BooleanTerm]) =>

            Ast.Xor(x.toList)
        }
      }

    def `boolean-function-NOT`: Rule1[Ast.Not] =
      rule {

        "not(" ~ `boolean-term` ~ ")" ~> {
          (x: Ast.BooleanTerm) =>

            Ast.Not(x)
        }
      }

    def `boolean-exists`: Rule1[Ast.Exists] =
      rule {

        `path` ~ "?exists" ~> {
          (x: Ast.Path) =>

            Ast.Exists(x)
        }
      }

    def `boolean-cmp-eq`: Rule1[Ast.Equal] =
      rule {

        (`path` ~ '?' ~ `sequence-mode` ~ `text-mode-opt` ~ "eq(" ~ `boolean-cmp-value-text` ~ ')' |
          `path` ~ "?ref." ~ `sequence-mode` ~ `text-mode-opt` ~ "eq(" ~ `boolean-cmp-value-path` ~ ')') ~> {
          (operand1: Ast.Path,
           sequenceMode: Ast.SequenceMode.Value,
           textMode: Ast.TextMode.Value,
           operand2: Either[Ast.Value, Ast.Path]) =>

            Ast.Equal(operand1, operand2, sequenceMode, textMode)
        }
      }

    def `boolean-cmp-ne`: Rule1[Ast.NotEqual] =
      rule {

        (`path` ~ '?' ~ `sequence-mode` ~ `text-mode-opt` ~ "ne(" ~ `boolean-cmp-value-text` ~ ')' |
          `path` ~ "?ref." ~ `sequence-mode` ~ `text-mode-opt` ~ "ne(" ~ `boolean-cmp-value-path` ~ ')') ~> {
          (operand1: Ast.Path,
           sequenceMode: Ast.SequenceMode.Value,
           textMode: Ast.TextMode.Value,
           operand2: Either[Ast.Value, Ast.Path]) =>

            Ast.NotEqual(operand1, operand2, sequenceMode, textMode)
        }
      }

    def `boolean-cmp-gt`: Rule1[Ast.GreaterThan] =
      rule {

        (`path` ~ '?' ~ `sequence-mode` ~ `text-mode-opt` ~ "gt(" ~ `boolean-cmp-value-text` ~ ')' |
          `path` ~ "?ref." ~ `sequence-mode` ~ `text-mode-opt` ~ "gt(" ~ `boolean-cmp-value-path` ~ ')') ~> {
          (operand1: Ast.Path,
           sequenceMode: Ast.SequenceMode.Value,
           textMode: Ast.TextMode.Value,
           operand2: Either[Ast.Value, Ast.Path]) =>

            Ast.GreaterThan(operand1, operand2, sequenceMode, textMode)
        }
      }

    def `boolean-cmp-ge`: Rule1[Ast.GreaterThanOrEqual] =
      rule {

        (`path` ~ '?' ~ `sequence-mode` ~ `text-mode-opt` ~ "ge(" ~ `boolean-cmp-value-text` ~ ')' |
          `path` ~ "?ref." ~ `sequence-mode` ~ `text-mode-opt` ~ "ge(" ~ `boolean-cmp-value-path` ~ ')') ~> {
          (operand1: Ast.Path,
           sequenceMode: Ast.SequenceMode.Value,
           textMode: Ast.TextMode.Value,
           operand2: Either[Ast.Value, Ast.Path]) =>

            Ast.GreaterThanOrEqual(operand1, operand2, sequenceMode, textMode)
        }
      }

    def `boolean-cmp-lt`: Rule1[Ast.LessThan] =
      rule {

        (`path` ~ '?' ~ `sequence-mode` ~ `text-mode-opt` ~ "lt(" ~ `boolean-cmp-value-text` ~ ')' |
          `path` ~ "?ref." ~ `sequence-mode` ~ `text-mode-opt` ~ "lt(" ~ `boolean-cmp-value-path` ~ ')') ~> {
          (operand1: Ast.Path,
           sequenceMode: Ast.SequenceMode.Value,
           textMode: Ast.TextMode.Value,
           operand2: Either[Ast.Value, Ast.Path]) =>

            Ast.LessThan(operand1, operand2, sequenceMode, textMode)
        }
      }

    def `boolean-cmp-le`: Rule1[Ast.LessThanOrEqual] =
      rule {

        (`path` ~ '?' ~ `sequence-mode` ~ `text-mode-opt` ~ "le(" ~ `boolean-cmp-value-text` ~ ')' |
          `path` ~ "?ref." ~ `sequence-mode` ~ `text-mode-opt` ~ "le(" ~ `boolean-cmp-value-path` ~ ')') ~> {
          (operand1: Ast.Path,
           sequenceMode: Ast.SequenceMode.Value,
           textMode: Ast.TextMode.Value,
           operand2: Either[Ast.Value, Ast.Path]) =>

            Ast.LessThanOrEqual(operand1, operand2, sequenceMode, textMode)
        }
      }

    def `boolean-cmp-ct`: Rule1[Ast.Contains] =
      rule {

        (`path` ~ '?' ~ `sequence-mode` ~ `text-mode-case` ~ "ct(" ~ `boolean-cmp-value-text` ~ ')' |
          `path` ~ "?ref." ~ `sequence-mode` ~ `text-mode-case` ~ "ct(" ~ `boolean-cmp-value-path` ~ ')') ~> {
          (operand1: Ast.Path,
           sequenceMode: Ast.SequenceMode.Value,
           textMode: Ast.TextMode.Value,
           operand2: Either[Ast.Value, Ast.Path]) =>

            Ast.Contains(operand1, operand2, sequenceMode, textMode)
        }
      }

    def `boolean-cmp-sw`: Rule1[Ast.StartsWith] =
      rule {

        (`path` ~ '?' ~ `sequence-mode` ~ `text-mode-case` ~ "sw(" ~ `boolean-cmp-value-text` ~ ')' |
          `path` ~ "?ref." ~ `sequence-mode` ~ `text-mode-case` ~ "sw(" ~ `boolean-cmp-value-path` ~ ')') ~> {
          (operand1: Ast.Path,
           sequenceMode: Ast.SequenceMode.Value,
           textMode: Ast.TextMode.Value,
           operand2: Either[Ast.Value, Ast.Path]) =>

            Ast.StartsWith(operand1, operand2, sequenceMode, textMode)
        }
      }

    def `boolean-cmp-ew`: Rule1[Ast.EndsWith] =
      rule {

        (`path` ~ '?' ~ `sequence-mode` ~ `text-mode-case` ~ "ew(" ~ `boolean-cmp-value-text` ~ ')' |
          `path` ~ "?ref." ~ `sequence-mode` ~ `text-mode-case` ~ "ew(" ~ `boolean-cmp-value-path` ~ ')') ~> {
          (operand1: Ast.Path,
           sequenceMode: Ast.SequenceMode.Value,
           textMode: Ast.TextMode.Value,
           operand2: Either[Ast.Value, Ast.Path]) =>

            Ast.EndsWith(operand1, operand2, sequenceMode, textMode)
        }
      }

    def `boolean-cmp-mt`: Rule1[Ast.Matches] =
      rule {

        (`path` ~ '?' ~ `sequence-mode` ~ "mt(" ~ `boolean-cmp-value-text` ~ ')' |
          `path` ~ "?ref." ~ `sequence-mode` ~ "mt(" ~ `boolean-cmp-value-path` ~ ')') ~> {
          (operand1: Ast.Path,
           sequenceMode: Ast.SequenceMode.Value,
           operand2: Either[Ast.Value, Ast.Path]) =>

            Ast.Matches(operand1, operand2, sequenceMode, Ast.TextMode.None)
        }
      }

    def `sequence-mode`: Rule1[Ast.SequenceMode.Value] =
      rule {

        "all." ~ push(Ast.SequenceMode.AtAll) |
          "in." ~ push(Ast.SequenceMode.AtLeastOne) |
          push(Ast.SequenceMode.None)
      }

    def `text-mode-opt`: Rule1[Ast.TextMode.Value] =
      rule {

        "txs." ~ push(Ast.TextMode.Sensitive) |
          "txi." ~ push(Ast.TextMode.Insensitive) |
          "txl." ~ push(Ast.TextMode.Length) |
          push(Ast.TextMode.None)
      }

    def `text-mode-case`: Rule1[Ast.TextMode.Value] =
      rule {

        "txs." ~ push(Ast.TextMode.Sensitive) |
          "txi." ~ push(Ast.TextMode.Insensitive) |
          push(Ast.TextMode.None)
      }

    def `boolean-cmp-value-path`: Rule1[Either[Ast.Value, Ast.Path]] =
      rule {

        `path` ~> ((x: Ast.Path) => Right(x))
      }

    def `boolean-cmp-value-text`: Rule1[Either[Ast.Value, Ast.Path]] =
      rule {

        `value-text` ~> ((x: Ast.Value) => Left(x))
      }

    def `value-key`: Rule1[String] =
      rule {

        capture(oneOrMore(CharSeq1))
      }

    def `value-field`: Rule1[String] =
      rule {

        capture(oneOrMore(CharSeq2))
      }

    def `value-text`: Rule1[Ast.Value] =
      rule {

        ('\'' ~ '\'' ~ '\'' ~ capture(zeroOrMore(!('\'' ~ '\'' ~ '\'') ~ ANY)) ~ '\'' ~ '\'' ~ '\'' |
          capture(zeroOrMore(!(')') ~ ANY))) ~> ((x: String) => Ast.Value(x))
      }

    def `value-integer`: Rule1[Int] =
      rule {

        capture((anyOf("+-") | MATCH) ~ (CharPredicate.Digit19 ~ zeroOrMore(CharPredicate.Digit) | '0')) ~>
          ((x: String) => BigInt(x)) ~> ((x: BigInt) => test(x <= Int.MaxValue || x >= Int.MinValue) ~ push(x.toInt))
      }
  }

  class Filter(ql: ObjectQueryLanguage, initials: List[AnyRef]) {

    def doPagination(): Pagination =
      new Pagination(ql, initials)

    def filter(clause: String): Filter = {

      val properties = new ObjectQueryLanguageParser(clause).`filter-expression`.run().get

      def search(obj: AnyRef): Boolean =
        !properties
          .exists {
            property =>

              if (property.rootKey.exists(!ql.checkRootKey(obj, _)))
                true
              else {

                val resolve = (path: Ast.Path) => {

                  val init = path.init.foldLeft(Option(obj))((a, b) => a.flatMap(ql.getObject(_, b)))

                  init.flatMap(ql.getValue(_, path.last))
                }

                !property.term.evaluate(resolve).get
              }
          }

      new Filter(ql, initials.filter(search))
    }
  }

  class Pagination(ql: ObjectQueryLanguage, initials: List[AnyRef]) {

    def doResolution(): Resolution =
      new Resolution(ql, initials, Nil)

    def skip(clause: String): Pagination = {

      val n = new ObjectQueryLanguageParser(clause).`skip-expression`.run().get

      skip(n)
    }

    def skip(n: Int): Pagination = {

      def f(seq: List[AnyRef]): List[AnyRef] =
        if (n < 0) seq.dropRight(-n) else seq.drop(n)

      new Pagination(ql, f(initials))
    }

    def limit(clause: String): Pagination = {

      val n = new ObjectQueryLanguageParser(clause).`limit-expression`.run().get

      limit(n)
    }

    def limit(n: Int): Pagination = {

      def f(seq: List[AnyRef]): List[AnyRef] =
        if (n < 0) seq.takeRight(-n) else seq.take(n)

      new Pagination(ql, f(initials))
    }

    def order(clause: String): Pagination = {

      val properties = new ObjectQueryLanguageParser(clause).`order-expression`.run().get

      val queue = mutable.Queue[AnyRef]()

      val resolve = (path: Ast.Path) => {

        val init = path.init.foldLeft(Option(queue.dequeue()))((a, b) => a.flatMap(ql.getObject(_, b)))

        init.flatMap(ql.getValue(_, path.last))
      }

      val sorted =
        properties.foldLeft(initials) {
          (initials, property) =>

            def f(obj1: AnyRef, obj2: AnyRef): Boolean = {

              val booleanTerm =
                if (property.ascending)
                  Ast.LessThan(property.path, Right(property.path), Ast.SequenceMode.None, property.textMode)
                else
                  Ast.GreaterThan(property.path, Right(property.path), Ast.SequenceMode.None, property.textMode)

              queue.enqueue(obj1, obj2)

              booleanTerm.evaluate(resolve).get
            }

            initials.sortWith(f)
        }

      new Pagination(ql, sorted)
    }
  }

  class Resolution(ql: ObjectQueryLanguage, initials: List[AnyRef], includes: List[AnyRef]) {

    def fetchResult(): Result =
      new Result(ql, initials, includes)

    def resolve(clause: String): Resolution = {

      val properties = new ObjectQueryLanguageParser(clause).`resolve-expression`.run().get

      def search(obj: AnyRef): List[AnyRef] =
        properties
          .map {
            property =>

              val init = property.path.init.foldLeft(Option(obj))((a, b) => a.flatMap(ql.getObject(_, b)))

              val last = init.flatMap(ql.getObject(_, property.path.last))

              val seq =
                last match {
                  case Some(x: util.Collection[_]) => x.asScala.toList.map(_.asInstanceOf[AnyRef])
                  case Some(x: Seq[_]) => x.map(_.asInstanceOf[AnyRef])
                  case Some(x: Array[_]) => x.toSeq.map(_.asInstanceOf[AnyRef])
                  case Some(x) => List(x)
                  case None => Nil
                }

              (seq, property.rootKey)
          }
          .filter {
            case (_, Some(rootKey)) =>
              ql.checkRootKey(obj, rootKey)
            case (_, None) =>
              true
          }
          .flatMap(_._1)

      def resolve(): List[AnyRef] = {

        def distinct(seq: List[AnyRef], acc: List[AnyRef]): List[AnyRef] =
          seq match {
            case x :: xs =>
              if (acc.contains(x))
                distinct(xs, acc)
              else
                distinct(xs, acc :+ x)
            case Nil =>
              acc
          }

        def collect(seq: List[AnyRef]): List[AnyRef] = {

          val result =
            distinct(seq ++ seq.flatMap(search), Nil)
              .filterNot(x => initials.contains(x) || includes.contains(x))

          if (seq.sameElements(result))
            seq
          else
            collect(result)
        }

        collect(initials ++ includes)
      }

      new Resolution(ql, initials, resolve())
    }
  }

  class Result(ql: ObjectQueryLanguage, val initials: List[AnyRef], val includes: List[AnyRef])

}
