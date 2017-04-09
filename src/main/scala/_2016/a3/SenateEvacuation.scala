package _2016.a3

import scala.io.Source

import java.io.FileOutputStream

object SenateEvacuation extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .grouped(2).map(_.last.split(' ').map(_.toInt)).zipWithIndex

  val parties = 'A' to 'Z'
  Console.withOut(new FileOutputStream(out)) {
    for ((senators, t) <- input) {
      val n = senators.length
      var total = senators.sum
      var evacuated = 0
      print(s"Case #${t + 1}:")
      while (total > n) {
        if (evacuated % 2 == 0) print(' ')
        val evac = senators.indices maxBy senators
        senators(evac) -= 1
        total -= 1
        evacuated += 1
        print(parties(evac))
      }

      for (evac <- 0 until n - 2) {
        if (evacuated % 2 == 0) print(' ')
        evacuated += 1
        print(parties(evac))
      }

      println(parties.slice(n - 2, n).mkString(" ", "", ""))
    }
  }
}

