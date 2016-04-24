import scala.io.StdIn

// Yeahhh
case class Lasso(spoke: Int, knot: Int, loop: Int)
object Lasso {
  def from(init: Int, memo: Array[Lasso])
          (step: Int => Int): Lasso = {

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
      if (memo(slow) != null)
        return extendSpoke(memo(slow))
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

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null).grouped(2)
  .map(_.last.split(' ').map(_.toInt - 1).view)
  .zipWithIndex.foreach { case (friends, t) =>
    val pairs = friends.zipWithIndex.filter {
      case (bff, kid) => friends(bff) == kid
    }

    val lassos = new Array[Lasso](friends.size)
    for (kid <- friends.indices) if (lassos(kid) == null)
      lassos(kid) = Lasso.from(kid, lassos)(friends)
    val widestLoop = lassos.view.map(_.loop).max
    val lassos2 = lassos.filter(_.loop == 2)
    def longestSpoke(kid: Int) = lassos2.view
      .filter(_.knot == kid).map(_.spoke).max
    val segments = for ((bff, kid) <- pairs)
      yield longestSpoke(kid) + 2 + longestSpoke(bff)
    val circle = widestLoop max (segments.sum / 2)
    println(s"Case #${t + 1}: $circle")
  }
