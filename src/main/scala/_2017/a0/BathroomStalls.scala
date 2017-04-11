package _2017.a0

import scala.io.Source

import java.lang.Long.numberOfLeadingZeros
import java.io.FileOutputStream

object BathroomStalls extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1).takeWhile(_ != null)
    .map(_.split(' ').view.map(_.toLong)).zipWithIndex

  Console.withOut(new FileOutputStream(out))(for {
    (Seq(n, k), t) <- input
    pow2 = 1l << (63 - numberOfLeadingZeros(k + 1))
    ceil = if (pow2 <= k) pow2 << 1 else pow2
    min  = (n - k) / ceil
    max  = if ((n - k) % ceil * 2 < ceil) min else min + 1
  } println(s"Case #${t + 1}: $max $min"))
}
