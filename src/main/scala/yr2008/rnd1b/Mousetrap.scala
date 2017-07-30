package yr2008.rnd1b

import scala.io.Source

import java.io.FileOutputStream

object Mousetrap extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .grouped(2).map(_.flatMap(_ split ' ').map(_.toInt)).zipWithIndex

  Console.withOut(new FileOutputStream(out)) {
    for ((Seq(k, n, indices @ _*), test) <- input) {
      val queries = indices.toArray
      val answers = Array.fill(n)(0)
      var pos = 0
      for (i <- 1 to k) {
        pos = (pos + i - 1) % (k - i + 1)
        for (j <- 0 until n) if (answers(j) <= 0)
          if (queries(j) == pos + 1) {
            queries(j) = 0
            answers(j) = i
          } else if (queries(j) > pos + 1) {
            queries(j) -= 1
          }
      }

      println(s"Case #${test + 1}: ${answers mkString " "}")
    }
  }
}
