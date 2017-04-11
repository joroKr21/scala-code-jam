package _2016.a0

import scala.io.Source

import java.io.FileOutputStream

object PancakeRevenge extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input =  Source.fromFile(in).getLines().drop(1).takeWhile(_ != null).zipWithIndex

  Console.withOut(new FileOutputStream(out))(for {
    (stack, t) <- input
    flips = stack.iterator.sliding(2).count {
      p => p.head != p.last
    } + (stack.last == '-').compare(false)
  } println(s"Case #${t + 1}: $flips"))
}
