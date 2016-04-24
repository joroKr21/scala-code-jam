import scala.io.StdIn

Iterator.continually(StdIn.readLine)
  .drop(1).takeWhile(_ != null).zipWithIndex
  .foreach { case (line, t) =>
    val rev = line.split(' ')
      .view.reverse.mkString(" ")

    println(s"Case #${t + 1}: $rev")
  }
