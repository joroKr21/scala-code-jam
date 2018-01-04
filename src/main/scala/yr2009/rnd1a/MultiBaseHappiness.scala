package yr2009.rnd1a

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

import java.io.FileOutputStream

object MultiBaseHappiness extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .map(_.split(' ').toVector.map(_.toInt)).zipWithIndex

  val limit = 1e3.toInt
  val bases = (2 to 10).toVector
  val maybeHappy = Array.fill(bases.last + 1)(mutable.BitSet.empty)
  val happy = mutable.Map(Vector.empty[Int] -> 2)

  def memo[A, R](f: A => R, cache: mutable.Map[A, R] = mutable.Map.empty[A, R]): A => R =
    arg => cache.getOrElseUpdate(arg, f(arg))

  def digits(num: Int, base: Int) = {
    val digits = Array.fill(base)(0)
    for (d <- Integer.toString(num, base)) digits(d.asDigit) += 1
    digits.toSeq
  }

  val sqrDigitSum = memo[Seq[Int], Int] {
    _.iterator.zipWithIndex.map { case (n, d) => n * d * d }.sum
  }

  val sqrDigitChain = memo[(Int, Int), Int] { case (num, base) =>
    val seen = mutable.BitSet.empty
    Iterator.iterate(num)(sqrDigitSum.compose(digits(_, base))).dropWhile {
      num => val notSeen = !seen(num); seen += num; notSeen
    }.next
  }

  @tailrec def isHappy(num: Int, base: Int): Boolean = {
    if (num <= limit) maybeHappy(base)(num)
    else isHappy(sqrDigitSum(digits(num, base)), base)
  }

  for { // pre-compute small happy numbers
    base <- bases
    num <- 2 to limit
    if sqrDigitChain(num, base) == 1
  } maybeHappy(base) += num

  for { // pre-compute all happy numbers
    n <- bases.indices
    b <- bases combinations n + 1
    init = b.indices.map { i =>
      val (pre, post) = b.splitAt(i)
      pre ++ post.tail
    }.map(happy).max
  } happy(b) = Iterator.from(init).find {
    num => b.forall(isHappy(num, _))
  }.get

  Console.withOut(new FileOutputStream(out)) {
    for ((bases, test) <- input)
      println(s"Case #${test + 1}: ${happy(bases)}")
  }
}
