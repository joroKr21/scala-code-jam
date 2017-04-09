package _2016.qual

import scala.io.Source

import java.io.FileOutputStream

object CountingSheep extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1)
    .takeWhile(_ != null).map(_.toInt).zipWithIndex

  Console.withOut(new FileOutputStream(out))(for {
    (step, t) <- input
    t1 = t + 1
  } if (step == 0) {
    println(s"Case #$t1: INSOMNIA")
  } else {
    var sheep  = step
    var digits = 0
    while (digits != 0x3ff) {
      var current = sheep
      while (current != 0) {
        digits |= 1 << (current % 10)
        current /= 10
      }

      sheep += step
    }

    sheep -= step
    println(s"Case #$t1: $sheep")
  })
}
