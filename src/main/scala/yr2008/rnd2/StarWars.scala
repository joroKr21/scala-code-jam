package yr2008.rnd2

import scala.io.Source

import java.io.FileOutputStream

object StarWars extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)

  Console.withOut(new FileOutputStream(out)) {
    var test = 0
    while (input.hasNext) {
      test += 1
      val n = input.next.toInt
      val ships = for (line <- input.take(n).toSeq)
        yield for (x <- line split ' ') yield x.toDouble

      var (lo, hi) = (0.0, 1e7)
      while (lo + 1e-7 < hi) {
        val power = (lo + hi) / 2
        val ranges = for (k <- 0 to 3; idx <- 0 to 2 combinations k)
          yield ships.map { ship =>
            ship.init.sum - 2 * idx.map(ship).sum + power * ship.last
          }.min

        val viable = ranges zip ranges.reverse forall {
          case (from, upto) => from + upto >= 0
        }

        if (viable) hi = power
        else lo = power
      }

      println(s"Case #$test: $hi")
    }
  }
}
