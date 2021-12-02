import cats.effect.IO

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

package object aoc {
  def load(filename: String): IO[List[String]] =
    IO(Files.readAllLines(Paths.get(filename)))
      .map(_.asScala.toList)

  def loadLine(filename: String): IO[String] =
    for {
      lines <- load(filename)
      line <- lines match {
        case List(s) => IO.pure(s)
        case other   => IO.raiseError(new RuntimeException(s"Expected 1 line, got ${other.size}"))
      }
    } yield line

  implicit class EitherOps[A, B](private val e: Either[A, B]) extends AnyVal {
    @inline def unsafeGet(): B =
      e match {
        case Left(t: Throwable) => throw new NoSuchElementException("Either.unsafeGet on Left").initCause(t)
        case Left(s: String)    => throw new NoSuchElementException(s"Either.unsafeGet on Left: $s")
        case Left(_)            => throw new NoSuchElementException("Either.unsafeGet on Left")
        case Right(b)           => b
      }
  }

}
