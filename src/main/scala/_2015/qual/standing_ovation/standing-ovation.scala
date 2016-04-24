import scala.io.StdIn

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null).zipWithIndex
  .foreach { case (audience, t) =>
    val (_, friends) = audience
      .view.dropWhile(_ != ' ')
      .tail.map(_.asDigit).zipWithIndex
      .foldLeft(0, 0) { case ((standing, fs), (k, shy)) =>
        if (standing >= shy) (standing + k, fs)
        else (shy + k, fs + shy - standing)
      }

    println(s"Case #${t + 1}: $friends")
  }
