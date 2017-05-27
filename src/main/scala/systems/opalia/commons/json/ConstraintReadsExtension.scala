package systems.opalia.commons.json

import play.api.libs.json._
import scala.collection.TraversableLike
import scala.util.matching.Regex


object ConstraintReadsExtension {

  def max[T](maximum: T)(implicit reads: Reads[T], ord: Ordering[T]): Reads[T] =
    Reads.filterNot[T](JsonValidationError("error.maximum", maximum))(x => ord.gt(x, maximum))

  def max[T](maximum: Option[T])(implicit reads: Reads[T], ord: Ordering[T]): Reads[T] =
    maximum match {
      case Some(x) => max[T](x)
      case None => Reads[T](reads.reads)
    }

  def min[T](minimum: T)(implicit reads: Reads[T], ord: Ordering[T]): Reads[T] =
    Reads.filterNot[T](JsonValidationError("error.minimum", minimum))(x => ord.lt(x, minimum))

  def min[T](minimum: Option[T])(implicit reads: Reads[T], ord: Ordering[T]): Reads[T] =
    minimum match {
      case Some(x) => min[T](x)
      case None => Reads[T](reads.reads)
    }

  def distinct[T]()(implicit reads: Reads[T], f: T => TraversableLike[_, T]): Reads[T] =
    Reads.filterNot[T](JsonValidationError("error.distinct"))((x) => x.size != x.toSeq.distinct.size)

  def exactLength[T](length: Int)(implicit reads: Reads[T], f: T => TraversableLike[_, T]): Reads[T] =
    Reads.filterNot[T](JsonValidationError("error.exact_length", length))(_.size != length)

  def maxLength[T](length: Int)(implicit reads: Reads[T], f: T => TraversableLike[_, T]): Reads[T] =
    Reads.filterNot[T](JsonValidationError("error.maximum_length", length))(_.size > length)

  def minLength[T](length: Int)(implicit reads: Reads[T], f: T => TraversableLike[_, T]): Reads[T] =
    Reads.filterNot[T](JsonValidationError("error.minimum_length", length))(_.size < length)

  def maxStringLength[T](length: Int)(implicit reads: Reads[T]): Reads[T] =
    Reads.filterNot[T](JsonValidationError("error.maximum_length_as_string", length))(_.toString.length > length)

  def minStringLength[T](length: Int)(implicit reads: Reads[T]): Reads[T] =
    Reads.filterNot[T](JsonValidationError("error.minimum_length_as_string", length))(_.toString.length < length)

  def choice[T](seq: T*)(implicit reads: Reads[T]): Reads[T] =
    Reads.filterNot[T](JsonValidationError("error.choice", seq: _*))(x => !seq.contains(x))

  def singleLine()(implicit reads: Reads[String]): Reads[String] =
    Reads.filterNot[String](JsonValidationError("error.single_line"))(_.lines.length > 1)

  def trimmed()(implicit reads: Reads[String]): Reads[String] =
    Reads.filterNot[String](JsonValidationError("error.trimmed"))(x => x != x.trim)

  def pattern(regex: Regex)(implicit reads: Reads[String]): Reads[String] =
    Reads.pattern(regex)

  def pattern(regex: Option[Regex])(implicit reads: Reads[String]): Reads[String] =
    regex match {
      case Some(x) => Reads.pattern(x)
      case None => Reads[String](reads.reads)
    }
}
