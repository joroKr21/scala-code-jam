package yr2017.qual

import scala.io.Source

import java.io.FileOutputStream

object OversizedPancakeFlipper extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null).map(_ split ' ')
    .map { case Array(ps, k) => (ps.toCharArray, k.toInt) }.zipWithIndex

  Console.withOut(new FileOutputStream(out)) {
    for (((pancakes, k), t) <- input) {
      val n = pancakes.length
      var flips = 0
      for (i <- 0 to n - k if pancakes(i) == '-') {
        flips += 1
        for (j <- i until i + k)
          pancakes(j) = if (pancakes(j) == '-') '+' else '-'
      }

      val possible = n - k + 1 until n forall (pancakes(_) == '+')
      val answer   = if (possible) flips.toString else "IMPOSSIBLE"
      println(s"Case #${t + 1}: $answer")
    }
  }
}
