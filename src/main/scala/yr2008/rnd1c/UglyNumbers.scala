package yr2008.rnd1c

import scala.io.Source

import java.io.FileOutputStream

object UglyNumbers extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .map(_.map(_.asDigit)).zipWithIndex

  val primes = Seq(2, 3, 5, 7)
  val mod = primes.product

  Console.withOut(new FileOutputStream(out)) {
    for ((digits, test) <- input) {
      val dyn = Array.fill(digits.length + 1, mod)(0l)
      dyn(0)(0) = 1

      for {
        i <- digits.indices
        signum <- if (i == 0) Seq(1) else Seq(-1, 1)
        rest = i until digits.length
        runSum = rest.scanLeft(0)((s, j) => (s * 10 + digits(j)) % mod)
        (sum, j) <- runSum.tail zip rest
        k <- 0 until mod
      } dyn(j + 1)((k + signum * sum + mod) % mod) += dyn(i)(k)

      val ugly = for {
        k <- 0 until mod
        if primes.exists(k % _ == 0)
      } yield dyn(digits.length)(k)
      println(s"Case #${test + 1}: ${ugly.sum}")
    }
  }
}
