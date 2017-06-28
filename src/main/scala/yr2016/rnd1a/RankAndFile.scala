package yr2016.rnd1a

import scala.io.Source

import java.io.FileOutputStream

object RankAndFile extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)

  Console.withOut(new FileOutputStream(out)) {
    var t = 0
    while (input.hasNext) {
      t += 1
      val n = input.next.toInt
      val missing = input
        .take(2 * n - 1)
        .flatMap(_ split ' ')
        .map(_.toInt).toVector
        .groupBy(identity)
        .mapValues(_.size)
        .filter(_._2 % 2 != 0)
        .keys.toVector.sorted
      assert(missing.size == n)
      println(s"Case #$t: ${missing.mkString(" ")}")
    }
  }
}
