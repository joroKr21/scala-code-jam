package _2015.qual

import scala.io.Source

import java.io.FileOutputStream

object StandingOvation extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null).zipWithIndex

  Console.withOut(new FileOutputStream(out))(for {
    (audience, t) <- input
    (_, friends) = audience.iterator
      .dropWhile(_ != ' ').drop(1).map(_.asDigit).zipWithIndex
      .foldLeft(0, 0) { case ((standing, fs), (k, shy)) =>
        if (standing >= shy) (standing + k, fs)
        else (shy + k, fs + shy - standing)
      }
  } println(s"Case #${t + 1}: $friends"))
}
