package systems.opalia.commons.io

import java.io.{File, IOException}
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import scala.language.reflectiveCalls


object FileUtils {

  def using[A <: {def close() : Unit}, B](closeable: A)(f: A => B): B =
    try {

      f(closeable)

    } finally {

      closeable.close()
    }

  def deleteRecursively(target: File): Unit = {

    deleteRecursively(target.toPath)
  }

  def deleteRecursively(target: Path): Unit = {

    Files.walkFileTree(target, new SimpleFileVisitor[Path]() {

      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {

        Files.delete(file)
        FileVisitResult.CONTINUE
      }

      override def postVisitDirectory(directory: Path, e: IOException): FileVisitResult = {

        if (e != null)
          throw e

        Files.delete(directory)
        FileVisitResult.CONTINUE
      }
    })
  }

  def copyRecursively(source: File, target: File, options: CopyOption*): Unit = {

    copyRecursively(source.toPath, target.toPath)
  }

  def copyRecursively(source: Path, target: Path, options: CopyOption*): Unit = {

    Files.walkFileTree(source, new SimpleFileVisitor[Path]() {

      override def preVisitDirectory(directory: Path, attrs: BasicFileAttributes): FileVisitResult = {

        Files.createDirectories(target.resolve(source.relativize(directory)))
        FileVisitResult.CONTINUE
      }

      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {

        Files.copy(file, target.resolve(source.relativize(file)), options: _*)
        FileVisitResult.CONTINUE
      }
    })
  }

  def moveRecursively(source: File, target: File, options: CopyOption*): Unit = {

    moveRecursively(source.toPath, target.toPath)
  }

  def moveRecursively(source: Path, target: Path, options: CopyOption*): Unit = {

    Files.walkFileTree(source, new SimpleFileVisitor[Path]() {

      override def preVisitDirectory(directory: Path, attrs: BasicFileAttributes): FileVisitResult = {

        Files.createDirectories(target.resolve(source.relativize(directory)))
        FileVisitResult.CONTINUE
      }

      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {

        Files.move(file, target.resolve(source.relativize(file)), options: _*)
        FileVisitResult.CONTINUE
      }

      override def postVisitDirectory(directory: Path, e: IOException): FileVisitResult = {

        if (e != null)
          throw e

        Files.delete(directory)
        FileVisitResult.CONTINUE
      }
    })
  }
}
