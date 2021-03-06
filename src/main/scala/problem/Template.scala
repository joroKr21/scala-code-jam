package problem

import scala.io.Source

import java.io.FileOutputStream

object Template extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null).zipWithIndex

  Console.withOut(new FileOutputStream(out)) {
    for ((line, test) <- input) {
      println(s"Case #${test + 1}: $line")
    }
  }
}
