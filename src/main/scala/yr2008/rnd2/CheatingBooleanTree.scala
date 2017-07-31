package yr2008.rnd2

import scala.io.Source

import java.io.FileOutputStream

object CheatingBooleanTree extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .map(_.split(' ').map(_.toInt))

  val gates = Seq(
    Seq(Seq(0 -> 0), Seq(0 -> 1, 1 -> 0, 1 -> 1)),
    Seq(Seq(0 -> 0, 0 -> 1, 1 -> 0), Seq(1 -> 1)))

  Console.withOut(new FileOutputStream(out)) {
    var test = 0
    while (input.hasNext) {
      test += 1
      val Array(m, v) = input.next
      val nodes = input.take(m).toVector
      val min = Array.fill(m, 2)(m)
      val cut = (m - 1) / 2

      for (i <- cut until m)
        min(i)(nodes(i).head) = 0

      for {
        i <- cut - 1 to 0 by -1
        j = 2 * (i + 1)
        Array(g, c) = nodes(i)
        (k, p) <- if (c <= 0) Seq(g -> 0)
          else Seq(g -> 0, (1 - g) -> 1)
        v <- 0 to 1
      } min(i)(v) = min(i)(v) min gates(k)(v).map {
        case (a, b) => min(j - 1)(a) + min(j)(b) + p
      }.min

      val answer = if (min(0)(v) >= m) "IMPOSSIBLE" else min(0)(v).toString
      println(s"Case #$test: $answer")
    }
  }

  object Memo {
    import scala.collection.mutable.{Map => Cache}

    def apply[A, R](f: A => R, cache: Cache[A, R] = Cache.empty[A, R]): A => R =
      arg => cache.getOrElseUpdate(arg, f(arg))

    def recur[A, R](f: (A => R) => A => R, cache: Cache[A, R] = Cache.empty[A, R]): A => R = {
      lazy val recur: A => R = apply[A, R](f(recur)(_), cache)
      recur
    }
  }
}
