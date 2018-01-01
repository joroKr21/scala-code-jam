package yr2009.qual

import scala.annotation.tailrec
import scala.io.Source

import java.io.FileOutputStream

object Watersheds extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .map(_.split(' ').map(_.toInt))

  Console.withOut(new FileOutputStream(out)) {
    var test = 0
    while (input.hasNext) {
      test += 1
      val Array(h, w) = input.next
      val map = input.take(h).toArray
      val basin = Array.fill(h, w)('\0')
      val labels = Iterator.from('a'.toInt).map(_.toChar)

      def flow(path: List[(Int, Int)], lbl: Char): Unit =
        for ((i, j) <- path) basin(i)(j) = lbl

      @tailrec def label(i: Int, j: Int, path: List[(Int, Int)]): Unit =
        if (basin(i)(j) >= 'a') flow(path, basin(i)(j)) else {
          val (k, l) = Iterator((i, j), (i - 1, j), (i, j - 1), (i, j + 1), (i + 1, j))
            .filter { case (x, y) => x >= 0 && x < h && y >= 0 && y < w }
            .minBy { case (x, y) => map(x)(y) }

          val newPath = (i, j) :: path
          if (map(k)(l) < map(i)(j)) label(k, l, newPath)
          else flow(newPath, labels.next)
        }

      for (i <- 0 until h; j <- 0 until w) label(i, j, Nil)
      val answer = basin.iterator.map(_.mkString(" ")).mkString("\n")
      println(s"Case #$test:\n$answer")
    }
  }
}
