package yr2008.qual

import scala.io.Source

import java.io.FileOutputStream

object SavingTheUniverse extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)

  Console.withOut(new FileOutputStream(out)) {
    var test = 0
    while (input.hasNext) {
      test += 1
      val engines = input.take(input.next.toInt).toSet
      val queries = input.take(input.next.toInt)
      val switches = queries.foldLeft(0, engines) {
        case ((switch, eng), query) =>
          val available = eng - query
          if (available.nonEmpty) (switch, available)
          else (switch + 1, engines - query)
      }._1
      println(s"Case #$test: $switches")
    }
  }
}
