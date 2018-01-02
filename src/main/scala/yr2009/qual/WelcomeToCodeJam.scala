package yr2009.qual

import scala.io.Source

import java.io.FileOutputStream

object WelcomeToCodeJam extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null).zipWithIndex
  val welcome = "welcome to code jam"
  val indices = welcome.zipWithIndex.groupBy(_._1).map {
    case (let, idx) => let -> idx.map(_._2)
  }.withDefaultValue(IndexedSeq.empty)

  Console.withOut(new FileOutputStream(out)) {
    for ((line, test) <- input) {
      val freq = Array.fill(welcome.length)(0)
      for (let <- line.reverse; idx <- indices(let))
        if (idx + 1 >= welcome.length) freq(idx) += 1
        else freq(idx) = (freq(idx) + freq(idx + 1)) % 10000
      println(f"Case #${test + 1}%s: ${freq.head}%04d")
    }
  }
}
