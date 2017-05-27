package systems.opalia.commons.io

import java.nio.file.{Path, Paths}
import scala.collection.JavaConverters._


object Imports {

  implicit class JavaPathImprovements(path: Path)
    extends Iterable[Path]
      with Ordered[Path] {

    def absolute: Path =
      if (!path.isAbsolute)
        Paths.get("/").resolve(path)
      else
        path

    def relative: Path =
      if (path.isAbsolute)
        Paths.get("/").relativize(path)
      else
        path

    def common(that: Path): Path = {

      def build(a: List[Path], b: List[Path]): Path =
        (a, b) match {
          case (x +: xs, y +: ys) if (x == y) => x.resolve(build(xs, ys))
          case _ => Paths.get("")
        }

      if (path.isAbsolute == that.isAbsolute) {

        if (path.isAbsolute)
          build(this.toList, that.toList).absolute
        else
          build(this.toList, that.toList).relative

      } else
        Paths.get("")
    }

    def uncommon(that: Path): Path = {

      val commonSize = this.common(that).size

      if (commonSize == 0)
        path
      else if (commonSize == path.size)
        Paths.get("")
      else
        path.subpath(commonSize, path.size)
    }

    def iterator: Iterator[Path] = {

      if (path == Paths.get(""))
        Nil.iterator
      else
        path.iterator().asScala
    }

    def compare(that: Path): Int =
      path.compareTo(that)
  }

}
