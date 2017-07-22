package yr2008.rnd1a

import scala.io.Source

import java.io.FileOutputStream

object MinScalarProduct extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .grouped(3).map(_.tail).zipWithIndex

  Console.withOut(new FileOutputStream(out)) {
    for ((vectors, test) <- input) {
      val Seq(xs, ys) = for (v <- vectors) yield for (x <- v split ' ') yield x.toLong
      val minP = (for ((x, y) <- xs.sorted zip ys.sorted.reverse) yield x * y).sum
      println(s"Case #${test + 1}: $minP")
    }
  }
}
