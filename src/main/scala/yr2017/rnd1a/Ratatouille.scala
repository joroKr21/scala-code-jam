package yr2017.rnd1a

import scala.annotation.tailrec
import scala.io.Source

import java.io.FileOutputStream

object Ratatouille extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1)
    .takeWhile(_ != null).map(_.split(' ').toVector.map(_.toInt))

  @tailrec def kits(k: Int, sizes: Seq[Int],
    packages: Vector[(Int, Int)], ranges: Vector[List[(Int, Int)]]
  ): Int = sizes match {
    case Nil => k
    case s :: ss =>
      if (packages.forall { case (lo, hi) => lo <= s && s <= hi }) {
        if (ranges.exists(_.isEmpty)) k + 1
        else kits(k + 1, sizes, ranges.map(_.head), ranges.map(_.tail))
      } else {
        val i = packages.indexWhere(_._2 < s)
        if (i < 0) kits(k, ss, packages, ranges)
        else ranges(i) match {
          case Nil => k
          case r :: rs => kits(k, sizes,
            packages.updated(i, r), ranges.updated(i, rs))
        }
      }
  }

  Console.withOut(new FileOutputStream(out)) {
    var t = 0
    while (input.hasNext) {
      t += 1
      val n = input.next.head
      val recipe = input.next

      val ranges = for {
        (r, packages) <- recipe zip input.take(n).toVector
      } yield (for {
        p <- packages.toList
        lo = (10*p) / (11*r + 1) + 1
        hi = (10*p) / ( 9*r)
        if lo <= hi
      } yield (lo, hi)).sorted

      val sizes = (for {
        range <- ranges.toList
        (lo, _) <- range
      } yield lo).distinct.sorted

      val k = kits(0, sizes, Vector.fill(n)(0, 0), ranges)
      println(s"Case #$t: $k")
    }
  }
}