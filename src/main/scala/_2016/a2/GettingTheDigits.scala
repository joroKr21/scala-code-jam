package _2016.a2

import scala.io.Source

import java.io.FileOutputStream

object GettingTheDigits extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1)
    .takeWhile(_ != null).map(_.toLowerCase).zipWithIndex

  val digits = "0245678913".map(_.asDigit)
  val unique = "zwufxvgiot"
  val words  = Seq("zero", "two", "four", "five", "six", "seven", "eight", "nine", "one", "three")
  val dlw    = for (((d, l), w) <- digits zip unique zip words) yield (d, l, w)

  Console.withOut(new FileOutputStream(out))(for {
    (letters, t) <- input
    freq = Array.ofDim[Int](10)
    rest = dlw.foldLeft(letters) {
      case (left, (digit, letter, word)) =>
        val n = left.count(_ == letter)
        freq(digit) = n
        left.diff(word * n)
    }
    _ = assert(rest.isEmpty)
    phone = new String(for {
      (n, d) <- freq.zipWithIndex
      digit <- Seq.fill(n)(d)
    } yield (digit + '0').toChar)
  } println(s"Case #${t + 1}: $phone"))
}
