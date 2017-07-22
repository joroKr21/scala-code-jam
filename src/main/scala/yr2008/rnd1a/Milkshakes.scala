package yr2008.rnd1a

import scala.io.Source

import java.io.FileOutputStream

object Milkshakes extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)

  Console.withOut(new FileOutputStream(out)) {
    var test = 0
    while (input.hasNext) {
      test += 1
      val ms, cs    = input.next.toInt
      val batches   = Array.fill(ms)(0)
      var possible  = true
      var customers = for {
        cust <- input.take(cs).toList
        malt = cust.split(' ').drop(1).map(_.toInt)
          .grouped(2).toSeq.groupBy(_.last > 0)
          .mapValues(_.map(_.head).toSet)
          .withDefaultValue(Set.empty)
      } yield malt(true) -> malt(false)

      while (possible && customers.nonEmpty) {
        val (onlyMalt, rest) = customers.partition(_._2.isEmpty)
        if (onlyMalt.isEmpty) customers = Nil
        else if (onlyMalt.exists(_._1.isEmpty)) possible = false
        else {
          val malted = onlyMalt.flatMap(_._1).toSet
          for (m <- malted) batches(m - 1) = 1
          customers = for {
            (yes, no) <- rest
            if yes.isEmpty || !yes.subsetOf(malted)
          } yield (yes, no diff malted)
        }
      }

      val answer = if (possible) batches.mkString(" ") else "IMPOSSIBLE"
      println(s"Case #$test: $answer")
    }
  }
}
