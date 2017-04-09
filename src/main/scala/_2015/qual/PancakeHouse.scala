package _2015.qual

import scala.collection.immutable.TreeMap
import scala.io.Source

import java.io.FileOutputStream

object PancakeHouse extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1)
    .takeWhile(_ != null).grouped(2).map(_.last).zipWithIndex

  Console.withOut(new FileOutputStream(out))(for {
    (diners, t) <- input
    plates = TreeMap(diners
      .split(' ').view.map(_.toInt)
      .groupBy(identity).mapValues(_.size)
      .toSeq: _*
    )(Ordering.Int.reverse)
    minutes = (for (batch <- 1 to plates.firstKey) yield
      (for ((p, f) <- plates.iterator.takeWhile(_._1 > batch))
        yield (p - 1) / batch * f
      ).sum + batch
    ).min
  } println(s"Case #${t + 1}: $minutes"))
}
