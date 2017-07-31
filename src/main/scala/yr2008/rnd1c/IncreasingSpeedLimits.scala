package yr2008.rnd1c

import scala.io.Source

import java.io.FileOutputStream

object IncreasingSpeedLimits extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)

  Console.withOut(new FileOutputStream(out)) {
    var test = 0
    while (input.hasNext) {
      test += 1
      val Array(n, m, x, y, z) = input.next.split(' ').map(_.toInt)
      val A = input.take(m).map(_.toLong).toArray

      val signs = for {
        i <- 0 until n
        a = A(i % m)
        _ = A(i % m) = (x * a + y * (i + 1l)) % z
      } yield a

      val idx = signs.distinct.sorted.zipWithIndex.toMap
      val pow = 32 - Integer.numberOfLeadingZeros(n)
      val bit = new BinDexTree(1 << pow, 1000000007)

      for {
        sign <- signs.reverseIterator
        norm = idx.size - idx(sign)
      } bit(norm) = 1 + bit(norm - 1)
      println(s"Case #$test: ${bit.size}")
    }
  }

  class BinDexTree(max: Int, mod: Int) {
    val tree = Array.fill(max)(0)
    var size = 0

    def apply(i: Int): Int = {
      var idx = i
      var sum = 0
      while (idx > 0) {
        sum = (sum + tree(idx)) % mod
        idx -= (idx & -idx)
      }

      sum
    }

    def update(i: Int, n: Int): Unit = {
      size = (size + n) % mod
      var idx = i
      while (idx < max) {
        tree(idx) = (tree(idx) + n) % mod
        idx += (idx & -idx)
      }
    }
  }
}
