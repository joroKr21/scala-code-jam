import scala.collection.immutable.TreeMap
import scala.io.StdIn

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null)
  .grouped(2).map(_.last).zipWithIndex
  .foreach { case (diners, t) =>
  val plates = TreeMap(
    diners.split(' ')
      .view.map(_.toInt)
      .groupBy(identity)
      .mapValues(_.size)
      .toSeq: _*)(Ordering.Int.reverse)

  val minutes = 1.to(plates.firstKey)
    .view.map { batch =>
      plates.view.takeWhile(_._1 > batch).map {
        case (p, f) => (p - 1) / batch * f
      }.sum + batch
    }.min

  println(s"Case #${t + 1}: $minutes")
}
