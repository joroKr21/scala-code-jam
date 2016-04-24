import scala.io.StdIn

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null)
  .map(_.split(' ').map(_.toInt)).zipWithIndex
  .foreach { case (Array(omino, width, height), t) =>
    val min = width min height
    val max = width max height
    val gabrielWins = omino < 7 &&
      width * height % omino == 0 && {
        omino match {
          case 3 => min > 1
          case 4 => min > 2
          case 5 => min > 2 &&
            (min, max) != (3, 5)
          case 6 => min > 3
          case _ => true
        }
      }

    val winner = if (gabrielWins) "GABRIEL" else "RICHARD"
    println(s"Case #${t + 1}: $winner")
  }
