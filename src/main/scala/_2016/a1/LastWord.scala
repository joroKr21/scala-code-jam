package _2016.a1

import scala.io.Source

import java.io.FileOutputStream

object LastWord extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null).zipWithIndex

  Console.withOut(new FileOutputStream(out))(for {
    (word, t) <- input
    lastWord = new String(word.tail.foldLeft(Vector(word.head)) {
      (last, c) => if (c < last.head) last :+ c else c +: last
    }.toArray)
  } println(s"Case #${t + 1}: $lastWord"))
}
