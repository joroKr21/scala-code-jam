package yr2016.qual

import scala.io.Source

import java.io.FileOutputStream

object CoinJam extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .map(_.split(' ').view.map(_.toInt)).zipWithIndex

  // Don't get stuck on 1 candidate
  val hardLimit = BigInt(1e12.toLong)
  lazy val primes: Stream[Long] = 2 #:: range(3, 2).filter {
    n => primes.takeWhile(p => p * p <= n).forall(n % _ != 0)
  }

  Console.withOut(new FileOutputStream(out))(for {
    (Seq(length, coins), t) <- input
    _ = println(s"Case #${t + 1}:")
    jam = for {
      c <- candidates(length)
      divs = for (d <- 2 to 10) yield divisor(BigInt(c, d))
      if divs.forall(_.isDefined)
    } yield s"$c ${divs.flatten.mkString(" ")}"
  } jam take coins foreach println)

  def range(from: Long, by: Long = 1): Stream[Long] =
    from #:: range(from + by, by)

  def candidates(length: Int) = for {
    c <- (1l << (length - 1)) + 1 until 1l << length by 2
  } yield c.toBinaryString

  def divisor(n: BigInt) = {
    val limit = n min hardLimit
    primes.takeWhile(p => p * p <= limit).find(n % _ == 0)
  }
}
