package yr2017.rnd1b

import scala.io.Source

import java.io.FileOutputStream

object Steed2CruiseControl extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1)
    .takeWhile(_ != null).map(_.split(' ').map(_.toInt))

  Console.withOut(new FileOutputStream(out)) {
    var t = 0
    while (input.hasNext) {
      t += 1
      val Array(d, n) = input.next
      val time = input.take(n).map {
        case Array(k, s) => (d - k) / s.toDouble
      }.max
      println(s"Case #$t: ${d / time}")
    }
  }
}
