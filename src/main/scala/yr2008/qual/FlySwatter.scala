package yr2008.qual

import scala.math.Pi
import scala.io.Source

import java.io.FileOutputStream

object FlySwatter extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null).zipWithIndex
  val eps2  = 1e-12

  def intersect(r2: Double, x1: Double, y1: Double, x2: Double, y2: Double): Double =
    if (x1 * x1 + y1 * y1 >= r2) 0
    else if (x2 * x2 + y2 * y2 <= r2) (x2 - x1) * (y2 - y1)
    else if ((x2 - x1) * (y2 - y1) < eps2 * r2) (x2 - x1) * (y2 - y1) / 2
    else {
      val mx = (x1 + x2) / 2
      val my = (y1 + y2) / 2
      val ll = intersect(r2, x1, y1, mx, my)
      val lr = intersect(r2, mx, y1, x2, my)
      val ul = intersect(r2, x1, my, mx, y2)
      val ur = intersect(r2, mx, my, x2, y2)
      ll + lr + ul + ur
    }

  Console.withOut(new FileOutputStream(out)) {
    for ((line, test) <- input) {
      val Array(fly, rad, thick, width, gap) = for (x <- line split ' ') yield x.toDouble
      val hit = if (rad <= thick + fly || gap <= 2 * fly) 1 else {
        val in   = rad - thick - fly
        val in2  = in * in
        val step = gap + 2 * width
        val bot  = width + fly
        val top  = width + gap - fly
        val miss = for {
          x <- 0.0 until in by step
          y <- 0.0 until in by step
        } yield intersect(in2, x + bot, y + bot, x + top, y + top)
        val area = Pi * rad * rad
        (area - 4 * miss.sum) / area
      }

      println(s"Case #${test + 1}: $hit")
    }
  }
}
