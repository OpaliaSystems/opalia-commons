package systems.opalia.commons.scripting.oql

import java.util
import scala.collection.JavaConverters._
import scala.collection.mutable
import systems.opalia.commons.scripting.oql.Ast.TextMode
import systems.opalia.commons.utility.RegexParsersEx


abstract class ObjectQueryLanguage(objects: List[AnyRef]) {

  def doFilter(): ObjectQueryLanguage.Filter =
    new ObjectQueryLanguage.Filter(this, objects)

  protected def checkRootKey(obj: AnyRef, key: String): Boolean

  protected def getObject(obj: AnyRef, segment: Ast.Segment): Option[AnyRef]

  protected def getValue(obj: AnyRef, segment: Ast.Segment): Option[Any]
}

object ObjectQueryLanguage {

  private object Parser
    extends RegexParsersEx {

    def `filter-expression`: Parser[List[Ast.FilterProperty]] =
      `filter-property` ~ rep(";" ~> `filter-property`) ^^ {
        case head ~ tail =>

          head :: tail
      }

    def `resolve-expression`: Parser[List[Ast.ResolveProperty]] =
      `resolve-property` ~ rep(";" ~> `resolve-property`) ^^ {
        case head ~ tail =>

          head :: tail
      }

    def `order-expression`: Parser[List[Ast.OrderProperty]] =
      `order-property` ~ rep(";" ~> `order-property`) ^^ {
        case head ~ tail =>

          head :: tail
      }

    def `skip-expression`: Parser[Int] =
      `value-integer`

    def `limit-expression`: Parser[Int] =
      `value-integer`

    def `filter-property`: Parser[Ast.FilterProperty] =
      `filter-root-path` | `boolean-term` ^^ (x => Ast.FilterProperty(x, None))

    def `resolve-property`: Parser[Ast.ResolveProperty] =
      `resolve-root-path` | `path` ^^ (x => Ast.ResolveProperty(x, None))

    def `order-property`: Parser[Ast.OrderProperty] =
      ("(" ~> (
        ("+" ^^ (_ => true) | "-" ^^ (_ => false)) ~
          ("s" ^^ (_ => TextMode.Sensitive) | "i" ^^ (_ => TextMode.Insensitive) | "l" ^^ (_ => TextMode.Length)).?
        ) <~ ")") ~ `path` ^^ {
        case ascending ~ textMode ~ path =>

          Ast.OrderProperty(path, ascending, textMode.getOrElse(TextMode.None))
      }

    def `filter-root-path`: Parser[Ast.FilterProperty] =
      ("~" ~> `value-key`) ~ ("!(" ~> `boolean-term` <~ ")") ^^ {
        case key ~ term =>

          Ast.FilterProperty(term, Some(key))
      }

    def `resolve-root-path`: Parser[Ast.ResolveProperty] =
      ("~" ~> `value-key`) ~ ("!(" ~> `path` <~ ")") ^^ {
        case key ~ path =>

          Ast.ResolveProperty(path, Some(key))
      }

    def `path`: Parser[Ast.Path] =
      rep(`segment` <~ ".") ~ `segment` ^^ {
        case init ~ last =>

          Ast.Path(init, last)
      }

    def `segment`: Parser[Ast.Segment] =
      `key-and-field-segment` |
        `field-segment`

    def `key-and-field-segment`: Parser[Ast.KeyAndFieldSegment] =
      ("~" ~> `value-key`) ~ ("!" ~> `value-field`) ^^ {
        case key ~ field =>

          Ast.KeyAndFieldSegment(key, field)
      }

    def `field-segment`: Parser[Ast.FieldSegment] =
      `value-field` ^^ {
        field =>

          Ast.FieldSegment(field)
      }

    def `boolean-term`: Parser[Ast.BooleanTerm] =
      `boolean-function` | `boolean-exists` | `boolean-comparator`

    def `boolean-function`: Parser[Ast.BooleanFunction] =
      `boolean-function-AND` | `boolean-function-OR` | `boolean-function-XOR` | `boolean-function-NOT`

    def `boolean-comparator`: Parser[Ast.BooleanComparator] =
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

    def `boolean-function-AND`: Parser[Ast.And] =
      "and(" ~> `boolean-term` ~ rep1("," ~> `boolean-term`) <~ ")" ^^ {
        case (head ~ tail) => Ast.And(head :: tail)
      }

    def `boolean-function-OR`: Parser[Ast.Or] =
      "or(" ~> `boolean-term` ~ rep1("," ~> `boolean-term`) <~ ")" ^^ {
        case (head ~ tail) => Ast.Or(head :: tail)
      }

    def `boolean-function-XOR`: Parser[Ast.Xor] =
      "xor(" ~> `boolean-term` ~ rep1("," ~> `boolean-term`) <~ ")" ^^ {
        case (head ~ tail) => Ast.Xor(head :: tail)
      }

    def `boolean-function-NOT`: Parser[Ast.Not] =
      "not(" ~> `boolean-term` <~ ")" ^^ {
        x => Ast.Not(x)
      }

    def `boolean-exists`: Parser[Ast.Exists] =
      `path` <~ "?exists" ^^ {
        operand => Ast.Exists(operand)
      }

    def `boolean-cmp-eq`: Parser[Ast.Equal] =
      `path` ~ (("?" ~> (`sequence-mode` ~ `text-mode-opt`)) ~ ("eq(" ~> `boolean-cmp-value-text`) |
        ("?ref." ~> (`sequence-mode` ~ `text-mode-opt`)) ~ ("eq(" ~> `boolean-cmp-value-path`)) <~ ")" ^^ {
        case operand1 ~ (sequenceMode ~ textMode ~ operand2) =>
          Ast.Equal(operand1, operand2, sequenceMode, textMode)
      }

    def `boolean-cmp-ne`: Parser[Ast.NotEqual] =
      `path` ~ (("?" ~> (`sequence-mode` ~ `text-mode-opt`)) ~ ("ne(" ~> `boolean-cmp-value-text`) |
        ("?ref." ~> (`sequence-mode` ~ `text-mode-opt`)) ~ ("ne(" ~> `boolean-cmp-value-path`)) <~ ")" ^^ {
        case operand1 ~ (sequenceMode ~ textMode ~ operand2) =>
          Ast.NotEqual(operand1, operand2, sequenceMode, textMode)
      }

    def `boolean-cmp-gt`: Parser[Ast.GreaterThan] =
      `path` ~ (("?" ~> (`sequence-mode` ~ `text-mode-opt`)) ~ ("gt(" ~> `boolean-cmp-value-text`) |
        ("?ref." ~> (`sequence-mode` ~ `text-mode-opt`)) ~ ("gt(" ~> `boolean-cmp-value-path`)) <~ ")" ^^ {
        case operand1 ~ (sequenceMode ~ textMode ~ operand2) =>
          Ast.GreaterThan(operand1, operand2, sequenceMode, textMode)
      }

    def `boolean-cmp-ge`: Parser[Ast.GreaterThanOrEqual] =
      `path` ~ (("?" ~> (`sequence-mode` ~ `text-mode-opt`)) ~ ("ge(" ~> `boolean-cmp-value-text`) |
        ("?ref." ~> (`sequence-mode` ~ `text-mode-opt`)) ~ ("ge(" ~> `boolean-cmp-value-path`)) <~ ")" ^^ {
        case operand1 ~ (sequenceMode ~ textMode ~ operand2) =>
          Ast.GreaterThanOrEqual(operand1, operand2, sequenceMode, textMode)
      }

    def `boolean-cmp-lt`: Parser[Ast.LessThan] =
      `path` ~ (("?" ~> (`sequence-mode` ~ `text-mode-opt`)) ~ ("lt(" ~> `boolean-cmp-value-text`) |
        ("?ref." ~> (`sequence-mode` ~ `text-mode-opt`)) ~ ("lt(" ~> `boolean-cmp-value-path`)) <~ ")" ^^ {
        case operand1 ~ (sequenceMode ~ textMode ~ operand2) =>
          Ast.LessThan(operand1, operand2, sequenceMode, textMode)
      }

    def `boolean-cmp-le`: Parser[Ast.LessThanOrEqual] =
      `path` ~ (("?" ~> (`sequence-mode` ~ `text-mode-opt`)) ~ ("le(" ~> `boolean-cmp-value-text`) |
        ("?ref." ~> (`sequence-mode` ~ `text-mode-opt`)) ~ ("le(" ~> `boolean-cmp-value-path`)) <~ ")" ^^ {
        case operand1 ~ (sequenceMode ~ textMode ~ operand2) =>
          Ast.LessThanOrEqual(operand1, operand2, sequenceMode, textMode)
      }

    def `boolean-cmp-ct`: Parser[Ast.Contains] =
      `path` ~ (("?" ~> (`sequence-mode` ~ `text-mode-case`)) ~ ("ct(" ~> `boolean-cmp-value-text`) |
        ("?ref." ~> (`sequence-mode` ~ `text-mode-case`)) ~ ("ct(" ~> `boolean-cmp-value-path`)) <~ ")" ^^ {
        case operand1 ~ (sequenceMode ~ textMode ~ operand2) =>
          Ast.Contains(operand1, operand2, sequenceMode, textMode)
      }

    def `boolean-cmp-sw`: Parser[Ast.StartsWith] =
      `path` ~ (("?" ~> (`sequence-mode` ~ `text-mode-case`)) ~ ("sw(" ~> `boolean-cmp-value-text`) |
        ("?ref." ~> (`sequence-mode` ~ `text-mode-case`)) ~ ("sw(" ~> `boolean-cmp-value-path`)) <~ ")" ^^ {
        case operand1 ~ (sequenceMode ~ textMode ~ operand2) =>
          Ast.StartsWith(operand1, operand2, sequenceMode, textMode)
      }

    def `boolean-cmp-ew`: Parser[Ast.EndsWith] =
      `path` ~ (("?" ~> (`sequence-mode` ~ `text-mode-case`)) ~ ("ew(" ~> `boolean-cmp-value-text`) |
        ("?ref." ~> (`sequence-mode` ~ `text-mode-case`)) ~ ("ew(" ~> `boolean-cmp-value-path`)) <~ ")" ^^ {
        case operand1 ~ (sequenceMode ~ textMode ~ operand2) =>
          Ast.EndsWith(operand1, operand2, sequenceMode, textMode)
      }

    def `boolean-cmp-mt`: Parser[Ast.Matches] =
      `path` ~ (("?" ~> `sequence-mode`) ~ ("mt(" ~> `boolean-cmp-value-text` |
        ("?ref." ~> `sequence-mode`) ~ "mt(" ~> `boolean-cmp-value-path`)) <~ ")" ^^ {
        case operand1 ~ (sequenceMode ~ operand2) =>
          Ast.Matches(operand1, operand2, sequenceMode, Ast.TextMode.None)
      }

    def `sequence-mode`: Parser[Ast.SequenceMode.Value] =
      ("all." ^^ (_ => Ast.SequenceMode.AtAll) |
        "in." ^^ (_ => Ast.SequenceMode.AtLeastOne)).? ^^ {
        _.getOrElse(Ast.SequenceMode.None)
      }

    def `text-mode-opt`: Parser[Ast.TextMode.Value] =
      ("txs." ^^ (_ => Ast.TextMode.Sensitive) |
        "txi." ^^ (_ => Ast.TextMode.Insensitive) |
        "txl." ^^ (_ => Ast.TextMode.Length)).? ^^ {
        _.getOrElse(Ast.TextMode.None)
      }

    def `text-mode-case`: Parser[Ast.TextMode.Value] =
      ("txs." ^^ (_ => Ast.TextMode.Sensitive) |
        "txi." ^^ (_ => Ast.TextMode.Insensitive)).? ^^ {
        _.getOrElse(Ast.TextMode.None)
      }

    def `boolean-cmp-value-path`: Parser[Either[Ast.Value, Ast.Path]] =
      `path` ^^ (x => Right(x))

    def `boolean-cmp-value-text`: Parser[Either[Ast.Value, Ast.Path]] =
      `value-text` ^^ (x => Left(x))

    def `value-key`: Parser[String] =
      """([a-zA-Z0-9]|-|_)+""".r

    def `value-field`: Parser[String] =
      """([a-zA-Z0-9]|_)+""".r

    def `value-text`: Parser[Ast.Value] =
      ("\'\'\'" ~> """.+?(?=('''))""".r <~ "\'\'\'" | """.+?(?=(\)))""".r) ^^ (x => Ast.Value(x))

    def `value-integer`: Parser[Int] =
      """[+-]?(0|([1-9][0-9]*))""".r ^^ (_.toInt)

    def parseFilterClause(value: String): List[Ast.FilterProperty] =
      parseAll(`filter-expression`, value) match {
        case Success(result, _) =>
          result
        case failure: NoSuccess =>
          throw new IllegalArgumentException(s"Failed to parse filter clause in query.\n${failure.msg}")
      }

    def parseSkipClause(value: String): Int =
      parseAll(`skip-expression`, value) match {
        case Success(result, _) =>
          result
        case failure: NoSuccess =>
          throw new IllegalArgumentException(s"Failed to parse skip clause in query.\n${failure.msg}")
      }

    def parseLimitClause(value: String): Int =
      parseAll(`limit-expression`, value) match {
        case Success(result, _) =>
          result
        case failure: NoSuccess =>
          throw new IllegalArgumentException(s"Failed to parse limit clause in query.\n${failure.msg}")
      }

    def parseOrderClause(value: String): List[Ast.OrderProperty] =
      parseAll(`order-expression`, value) match {
        case Success(result, _) =>
          result
        case failure: NoSuccess =>
          throw new IllegalArgumentException(s"Failed to parse order clause in query.\n${failure.msg}")
      }

    def parseResolveClause(value: String): List[Ast.ResolveProperty] =
      parseAll(`resolve-expression`, value) match {
        case Success(result, _) =>
          result
        case failure: NoSuccess =>
          throw new IllegalArgumentException(s"Failed to parse resolve clause in query.\n${failure.msg}")
      }
  }

  class Filter(ql: ObjectQueryLanguage, initials: List[AnyRef]) {

    def doPagination(): Pagination =
      new Pagination(ql, initials)

    def filter(clause: String): Filter = {

      val properties = Parser.parseFilterClause(clause)

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

      val n = Parser.parseSkipClause(clause)

      skip(n)
    }

    def skip(n: Int): Pagination = {

      def f(seq: List[AnyRef]): List[AnyRef] =
        if (n < 0) seq.dropRight(-n) else seq.drop(n)

      new Pagination(ql, f(initials))
    }

    def limit(clause: String): Pagination = {

      val n = Parser.parseLimitClause(clause)

      limit(n)
    }

    def limit(n: Int): Pagination = {

      def f(seq: List[AnyRef]): List[AnyRef] =
        if (n < 0) seq.takeRight(-n) else seq.take(n)

      new Pagination(ql, f(initials))
    }

    def order(clause: String): Pagination = {

      val properties = Parser.parseOrderClause(clause)

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

      val properties = Parser.parseResolveClause(clause)

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
