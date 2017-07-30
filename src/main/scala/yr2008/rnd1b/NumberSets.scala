package yr2008.rnd1b

import scala.annotation.tailrec
import scala.io.Source
import scala.math.sqrt

import java.io.FileOutputStream

object NumberSets extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .map(_.split(' ').map(_.toLong)).zipWithIndex

  lazy val primes: Stream[Int] = 2 #:: Stream.from(3, 2)
    .filter(n => primes.takeWhile(_ <= sqrt(n)).forall(n % _ != 0))

  Console.withOut(new FileOutputStream(out)) {
    for ((Array(a, b, p), test) <- input) {
      val range = (b - a + 1).toInt
      val sets = if (range <= 0) 0
        else if (range <= p) range
        else {
          val ps = primes.dropWhile(_ < p).takeWhile(_ < range)
          val uf = new UnionFind(range)
          for {
            p <- ps
            s = (p - a % p).toInt % p
            q <- s + p until range by p
          } uf.unite(q, s)
          uf.size
        }

      println(s"Case #${test + 1}: $sets")
    }
  }

  class UnionFind(n: Int) {
    val id = Array.range(0, n)
    val sz = Array.fill(n)(1)
    var size = n

    @tailrec private def root(i: Int): Int =
      if (i == id(i)) i else {
        id(i) = id(id(i))
        root(id(i))
      }

    def unite(p: Int, q: Int): Unit = {
      val i = root(p)
      val j = root(q)
      if (i != j) {
        size -= 1
        if (sz(i) < sz(j)) {
          id(i) = j
          sz(j) += sz(i)
        } else {
          id(j) = i
          sz(i) += sz(j)
        }
      }
    }
  }
}
