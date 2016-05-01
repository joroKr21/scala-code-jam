import scala.io.StdIn

val digits = "0245678913".map(_.asDigit)
val unique = "zwufxvgiot"
val words = Seq(
  "zero", "two", "four", "five", "six",
  "seven", "eight", "nine", "one", "three")

val dlw = (digits zip unique zip words).map {
  case ((d, l), w) => (d, l, w)
}

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null)
  .map(_.toLowerCase).zipWithIndex
  .foreach { case (letters, t) =>
    val freq = new Array[Int](10)
    val rest = dlw.foldLeft(letters) {
      case (left, (digit, letter, word)) =>
        val n = left.count(_ == letter)
        freq(digit) = n
        left.diff(word * n)
    }

    assert(rest.isEmpty)
    val phone = freq
      .view.zipWithIndex
      .flatMap { case (n, digit) =>
        Seq.fill(n)(digit)
      }.mkString

    println(s"Case #${t + 1}: $phone")
  }
