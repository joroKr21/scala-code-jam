import scala.io.StdIn

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null)
  .grouped(2).map(_.last)
  .map(_.split(' ').map(_.toInt).view)
  .zipWithIndex
  .foreach { case (intervals, n) =>
    val (minVar, constRate) = intervals
      .sliding(2)
      .map(r => (r.head - r.last) max 0)
      .duplicate match {
        case (rates1, rates2) =>
          (rates1.sum, rates2.max)
      }

    val minConst = intervals.init.map(_ min constRate).sum
    println(s"Case #${n + 1}: $minVar $minConst")
  }
