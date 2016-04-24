import scala.io.StdIn

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null).zipWithIndex
  .foreach { case (stack, t) =>
    val flips = stack.view.sliding(2)
      .count(p => p.head != p.last) +
      truth(stack.last == '-')

    println(s"Case #${t + 1}: $flips")
  }

def truth(p: Boolean) =
  p.compare(false)
