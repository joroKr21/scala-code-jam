package yr2008.rnd1c

import scala.io.Source

import java.io.FileOutputStream

object TextMsgOutrage extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .grouped(2).map(_.flatMap(_ split ' ').map(_.toLong)).zipWithIndex

  Console.withOut(new FileOutputStream(out)) {
    for ((Seq(_, k, _, freq @ _*), test) <- input) {
      val minPress = freq.sorted.reverseIterator
        .grouped(k.toInt).zipWithIndex.flatMap {
          case (g, i) => for (f <- g) yield f * (i + 1)
        }.sum

      println(s"Case #${test + 1}: $minPress")
    }
  }
}
