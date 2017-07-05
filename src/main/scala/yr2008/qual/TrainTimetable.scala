package yr2008.qual

import scala.io.Source

import java.io.FileOutputStream

object TrainTimetable extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)

  Console.withOut(new FileOutputStream(out)) {
    var test = 0
    while (input.hasNext) {
      test += 1
      val turn = input.next.toInt
      val Array(na, nb) = for {
        n <- input.next split ' '
      } yield n.toInt

      val schedule = for {
        times <- input take na + nb
        Array(dep, arr) = for {
          t <- times split ' '
          Array(h, m) = for {
            x <- t split ':'
          } yield x.toInt
        } yield h * 60 + m
      } yield dep -> arr

      val (dep, arr) = schedule.toList.unzip
      val Seq(depart, arrive) = for {
        xss <- Seq(dep, arr)
        (xs, ys) = xss splitAt na
      } yield for {
        xs <- Seq(xs, ys)
      } yield xs.sorted

      val trains = for {
        (dep, arr) <- depart zip arrive.reverse
      } yield dep.foldLeft(0, arr) {
        case ((n, Nil), _) => (n + 1, Nil)
        case ((n, a :: as), d) if a + turn <= d => (n, as)
        case ((n, as), _) => (n + 1, as)
      }._1

      println(s"Case #$test: ${trains mkString " "}")
    }
  }
}
