package _2015.a0

import scala.io.Source

import java.io.FileOutputStream

object OminousOmino extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .map(_.split(' ').map(_.toInt)).zipWithIndex

  Console.withOut(new FileOutputStream(out))(for {
    (Array(omino, w, h), t) <- input
    (min, max) = (w min h, w max h)
    winner = if (omino < 7 && w * h % omino == 0 &&
      (omino match {
        case 3 => min > 1
        case 4 => min > 2
        case 5 => min > 2 && (min, max) != (3, 5)
        case 6 => min > 3
        case _ => true
      })) "GABRIEL" else "RICHARD"
  } println(s"Case #${t + 1}: $winner"))
}
