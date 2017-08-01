package yr2008.rnd2

import scala.io.Source

import java.io.FileOutputStream

object TriangleAreas extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .map(_.split(' ').map(_.toInt)).zipWithIndex

  Console.withOut(new FileOutputStream(out)) {
    for ((Array(n, m, a), test) <- input) {
      val k = a / m
      val triangle = if (a > m * n) "IMPOSSIBLE"
        else if (a < m) s"0 0 1 0 0 $a"
        else if (k == n) s"0 0 $n 0 0 $m"
        else s"0 ${a - k * m} $k 0 ${k + 1} $m"
      println(s"Case #${test + 1}: $triangle")
    }
  }
}
