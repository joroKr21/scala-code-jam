package _2016.a1

import scala.io.Source

import java.io.FileOutputStream

object Bffs extends App {
  // Yeahhh
  case class Lasso(spoke: Int, knot: Int, loop: Int)
  object Lasso {
    def from(init: Int, memo: Array[Lasso])(step: Int => Int): Lasso = {
      var slow = init
      var fast = init
      var spoke = 0
      var loop = 0

      def extendSpoke(lasso: Lasso) = {
        fast = init
        // Memoize along the spoke
        for (left <- spoke to 1 by -1) {
          memo(fast) = lasso.copy(spoke = lasso.spoke + left)
          fast = step(fast)
        }

        memo(init)
      }

      do { // Find loop
        if (memo(slow) != null) return extendSpoke(memo(slow))
        slow = step(slow)
        fast = step(step(fast))
        spoke += 1
      } while (slow != fast)

      do { // Calculate loop size
        loop += 1
        fast = step(fast)
      } while (slow != fast)

      do { // Memoize along the loop
        memo(fast) = Lasso(0, fast, loop)
        fast = step(fast)
      } while (slow != fast)

      slow = init
      fast = init
      spoke = 0
      // Find knot
      for (_ <- 1 to loop) fast = step(fast)
      while (slow != fast) {
        slow = step(slow)
        fast = step(fast)
        spoke += 1
      }

      extendSpoke(Lasso(0, fast, loop))
    }
  }

  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .grouped(2).map(_.last.split(' ').map(_.toInt - 1).view).zipWithIndex

  Console.withOut(new FileOutputStream(out))(for {
    (friends, t) <- input
    pairs = for {
      pair @ (bff, kid) <- friends.zipWithIndex
      if friends(bff) == kid
    } yield pair
    lassos = Array.ofDim[Lasso](friends.size)
    _ = for (kid <- friends.indices if lassos(kid) == null)
      lassos(kid) = Lasso.from(kid, lassos)(friends)
    widestLoop = (for (lasso <- lassos.iterator) yield lasso.loop).max
    lassos2    =  for (lasso <- lassos if lasso.loop == 2) yield lasso
  } {
    def longestSpoke(kid: Int) = (for {
      lasso <- lassos2.iterator
      if lasso.knot == kid
    } yield lasso.spoke).max
    val segments = for ((bff, kid) <- pairs)
      yield longestSpoke(kid) + 2 + longestSpoke(bff)
    val circle = widestLoop max (segments.sum / 2)
    println(s"Case #${t + 1}: $circle")
  })
}
