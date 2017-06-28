package yr2017.qual

import scala.io.Source

import java.io.FileOutputStream

object TidyNumbers extends App {
  val Array(in, out) = for (f <- args) yield s"src/main/resources/$f"
  val input = Source.fromFile(in).getLines().drop(1)
    .takeWhile(_ != null).map(_.toCharArray).zipWithIndex

  Console.withOut(new FileOutputStream(out)) {
    for ((digits, t) <- input) {
      val n = digits.length
      var (i, done) = (1, false)
      while (i < n && !done) if (digits(i - 1) > digits(i)) {
        for (j <- i until n) digits(j) = '9'
        var k = i - 1
        while (k > 0 && digits(k - 1) == digits(k))
          { digits(k) = '9'; k -= 1 }
        digits(k) = (digits(k) - 1).toChar
        done = true
      } else i += 1
      val tidy = new String(digits.dropWhile(_ == '0'))
      println(s"Case #${t + 1}: $tidy")
    }
  }
}
