package yr2015.rnd1a

import scala.io.Source

import java.io.FileOutputStream

object MushroomMonster extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .grouped(2).map(_.last).map(_.split(' ').map(_.toInt).view).zipWithIndex

  Console.withOut(new FileOutputStream(out))(for {
    (intervals, n) <- input
    rs = for (Seq(r1, r2) <- intervals.sliding(2))
      yield 0 max (r1 - r2)
    (rs1, rs2) = rs.duplicate
    constRate  = rs2.max
    minConst   = intervals.init.map(_ min constRate).sum
  } println(s"Case #${n + 1}: ${rs1.sum} $minConst"))
}
