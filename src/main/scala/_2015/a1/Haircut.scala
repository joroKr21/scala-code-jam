package _2015.a1

import scala.io.Source
import scala.util.control.Breaks._

import java.io.FileOutputStream

object Haircut extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .map(_.split(' ').map(_.toInt).view).grouped(2).zipWithIndex

  Console.withOut(new FileOutputStream(out))(for {
    (Seq(Seq(_, customers), barbers), t) <- input
    rate = barbers.map(1.0 / _).sum
  } {
    var barber = 0
    var time   = ((0 max (customers - barbers.size - 1)) / rate).toLong - 1
    val served = if (time < 0) 0 else barbers.map(time / _ + 1).sum
    var remain = customers - served
    while (remain > 0) {
      time += barbers.map(b => b - time % b).min
      breakable(for {
        (b, i) <- barbers.zipWithIndex
        if time % b == 0
        _ = remain -= 1
      } if (remain == 0) {
        barber = i + 1
        break()
      })
    }

    println(s"Case #${t + 1}: $barber")
  })
}
