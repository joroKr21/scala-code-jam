package _2010.a0

import scala.io.Source

import java.io.FileOutputStream

object ReverseWords extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null).zipWithIndex

  Console.withOut(new FileOutputStream(out))(for {
    (line, t) <- input
    rev = line.split(' ').reverseIterator.mkString(" ")
  } println(s"Case #${t + 1}: $rev"))
}
