package yr2010.qual

import scala.io.Source
import scala.util.control.Breaks._

import java.io.FileOutputStream

object StoreCredit extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1)
    .takeWhile(_ != null).grouped(3).zipWithIndex

  Console.withOut(new FileOutputStream(out))(for {
    (Seq(c, _, prices), t) <- input
    (items, credit, t1) = (Array.ofDim[Int](1000), c.toInt, t + 1)
  } breakable(for {
    (price, i) <- prices.split(' ').iterator.map(_.toInt).zipWithIndex
    (i1, p1, r) = (i + 1, price - 1, credit - price - 1)
  } if (r >= 0 && items(r) > 0) {
    println(s"Case #$t1: ${items(r)} $i1")
    break()
  } else if (items(p1) <= 0) {
    items(p1) = i1
  }))
}
