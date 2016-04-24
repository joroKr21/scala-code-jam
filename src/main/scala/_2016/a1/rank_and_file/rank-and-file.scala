import scala.io.StdIn

val input = Iterator
  .continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null)

var t = 0
while (input.hasNext) {
  t += 1
  val n = input.next.toInt
  val missing = input
    .take(2 * n - 1)
    .flatMap(_ split ' ')
    .map(_.toInt).toVector
    .groupBy(identity)
    .mapValues(_.size)
    .filter(_._2 % 2 != 0)
    .keys.toVector.sorted
  assert(missing.size == n)
  println(s"Case #$t: ${missing.mkString(" ")}")
}


