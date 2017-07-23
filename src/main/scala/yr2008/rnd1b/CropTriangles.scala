package yr2008.rnd1b

import scala.io.Source

import java.io.FileOutputStream

object CropTriangles extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .map(_.split(' ').map(_.toLong)).zipWithIndex

  Console.withOut(new FileOutputStream(out)) {
    for ((Array(n, a, b, c, d, x0, y0, m), test) <- input) {
      val buckets = Array.fill(9)(0l)

      for {
        (x, y) <- Iterator.iterate(x0, y0) {
          case (x, y) => ((a * x + b) % m, (c * y + d) % m)
        } take n.toInt
      } buckets((x % 3 * 3 + y % 3).toInt) += 1

      val sameBucket = for (n <- buckets)
        yield n * (n - 1) * (n - 2) / 6

      val diffBuckets = for {
        idx <- 0 until 9 combinations 3
        if idx.map(_ / 3).sum % 3 == 0
        if idx.map(_ % 3).sum % 3 == 0
      } yield idx.map(buckets).product

      val answer = sameBucket.sum + diffBuckets.sum
      println(s"Case #${test + 1}: $answer")
    }
  }
}
